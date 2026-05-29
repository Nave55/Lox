{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Bifunctor
import Text.Read (readMaybe)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>

-- data --

data TokenType
  = -- Single-character tokens
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR

  -- Double-character tokens
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL

  -- Literals
  | IDENTIFIER
  | STRING
  | NUMBER

  -- Keywords
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE

  | EOF
  deriving (Show)

data Literal
  = L_NUMBER Double
  | L_STRING BS.ByteString
  -- | L_BOOL   Bool
  -- | L_NIL
  deriving (Show)

data Token = Token
  { t_type    :: TokenType
  , t_lexeme  :: BS.ByteString
  , t_literal :: Maybe Literal
  , t_line    :: Int
  }
  deriving (Show)

data Scanner = Scanner
  { s_source :: BS.ByteString
  , s_tokens :: [Token]
  , s_errors :: [String]
  }
  deriving (Show)

data Loc = Loc
  { l_start   :: Int
  , l_current :: Int
  , l_line    :: Int
  }
  deriving (Show)

-- string and error functions

createError :: Int -> String -> String
createError line = report line ""

report :: Int -> String -> String -> String
report line loc msg = do
  "[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ msg

tokenToString :: Token -> String
tokenToString token =
  show (t_type token) ++ " " ++ show (t_lexeme token) ++ " " ++ show (t_literal token)

-- create records

createToken :: TokenType -> BS.ByteString -> Maybe Literal -> Int -> Token
createToken t_type t_lexeme t_literal t_line =
  Token { t_type, t_lexeme, t_literal, t_line }

initScanner :: String -> Scanner
initScanner s_source =
  Scanner
    { s_source = BS.pack s_source
    , s_tokens = []
    , s_errors = []
    }

createScanner :: String -> [Token] -> [String] -> Scanner
createScanner s_source s_tokens s_errors =
  Scanner
    { s_source = BS.pack s_source
    , s_tokens = s_tokens
    , s_errors = s_errors
    }

initLoc :: Loc
initLoc = Loc { l_start = 0, l_current = 0, l_line = 1 }

createLoc :: Int -> Int -> Int -> Loc
createLoc l_start l_current l_line =
  Loc { l_start, l_current, l_line }

-- basic functions

sliceBs :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBs i j bs = BS.take (j - i) (BS.drop i bs)

-- LEXER CORE

-- lexer helper functions

sliceStartCurrent :: Loc -> Scanner -> BS.ByteString
sliceStartCurrent loc scanner =
  sliceBs (l_start loc) (l_current loc) (s_source scanner)

sliceStartCurrentOff :: Loc -> Scanner -> Int -> Int -> BS.ByteString
sliceStartCurrentOff loc scanner s_off c_off =
  sliceBs (l_start loc + s_off) (l_current loc + c_off) (s_source scanner)

sourceLen :: Scanner -> Int
sourceLen scanner = BS.length $ s_source scanner

charAtCur :: Loc -> Scanner -> Char
charAtCur loc scanner = BS.index (s_source scanner) (l_current loc)

charAtCurOff :: Loc -> Scanner -> Int -> Char
charAtCurOff loc scanner off = BS.index (s_source scanner) (l_current loc + off)

isAtEnd :: Loc -> Scanner -> Bool
isAtEnd loc scanner = l_current loc >= sourceLen scanner

advance :: Loc -> Scanner -> (Loc, Char)
advance loc scanner =
  let c      = charAtCur loc scanner
      n_curr = l_current loc + 1
      loc1   = loc { l_current = n_curr }
  in (loc1, c)

peek :: Loc -> Scanner -> Char
peek loc scanner
  | isAtEnd loc scanner = '\0'
  | otherwise = charAtCur loc scanner

peekNext :: Loc -> Scanner -> Char
peekNext loc scanner
  | l_current loc + 1 >= BS.length (s_source scanner) = '\0'
  | otherwise = BS.index (s_source scanner) (l_current loc + 1)

match :: Loc -> Scanner -> Char -> (Loc, Bool)
match loc scanner expected
  | isAtEnd loc scanner = (loc, False)
  | charAtCur loc scanner /= expected = (loc, False)
  | otherwise = (loc {l_current = l_current loc + 1}, True)

keyword :: BS.ByteString -> Maybe TokenType
keyword "and"    = Just AND
keyword "class"  = Just CLASS
keyword "else"   = Just ELSE
keyword "false"  = Just FALSE
keyword "fun"    = Just FUN
keyword "for"    = Just FOR
keyword "if"     = Just IF
keyword "nil"    = Just NIL
keyword "or"     = Just OR
keyword "print"  = Just PRINT
keyword "return" = Just RETURN
keyword "super"  = Just SUPER
keyword "this"   = Just THIS
keyword "true"   = Just TRUE
keyword "var"    = Just VAR
keyword "while"  = Just WHILE
keyword _        = Nothing

addToken :: TokenType -> Loc -> Scanner -> Scanner
addToken t_type = addTokenWithLiteral t_type Nothing

addTokenWithLiteral :: TokenType -> Maybe Literal -> Loc -> Scanner -> Scanner
addTokenWithLiteral t_type literal loc scanner =
  let text  = sliceStartCurrent loc scanner
      token = createToken t_type text literal (l_line loc)
  in scanner { s_tokens = token : s_tokens scanner }

scanForSlashes :: Loc -> Scanner -> (Loc, Scanner)
scanForSlashes loc scanner
  | matched   = consumeComment loc1
  | otherwise = (loc, addTokenWithLiteral SLASH Nothing loc scanner)
  where
    (loc1, matched) = match loc scanner '/'

    consumeComment loc =
      let c = peek loc scanner
      in if c /= '\n' && not (isAtEnd loc scanner)
           then
             let (loc1, _) = advance loc scanner
             in consumeComment loc1
           else
             (loc, scanner)

scanForStrings :: Loc -> Scanner -> (Loc, Scanner)
scanForStrings loc scanner
  | peek loc scanner /= '"' && not (isAtEnd loc scanner) =
      let loc1 =
            if peek loc scanner == '\n'
              then loc { l_line = l_line loc + 1 }
              else loc

          (loc2, _) = advance loc1 scanner
      in scanForStrings loc2 scanner

  | isAtEnd loc scanner =
      let err = createError (l_line loc) "Unterminated String."
      in (loc, scanner { s_errors = err : s_errors scanner })

  | otherwise =
      let (loc1, _) = advance loc scanner
          val       = sliceStartCurrentOff loc1 scanner 1 (-1)
          lit       = Just (L_STRING val)
      in (loc1, addTokenWithLiteral STRING lit loc1 scanner)

scanForNumbers :: Loc -> Scanner -> (Loc, Scanner)
scanForNumbers loc scanner =
  let loc1 = consumeDigits loc

      loc2 =
        if peek loc1 scanner == '.' && isDigit (peekNext loc1 scanner)
          then fst (advance loc1 scanner)
          else loc1

      loc3 = consumeDigits loc2
  in
    (loc3, addTokenWithLiteral NUMBER (parseSourceToDouble loc3) loc3 scanner)

  where
    consumeDigits loc
      | isDigit (peek loc scanner) =
          let (loc1, _) = advance loc scanner
          in consumeDigits loc1
      | otherwise = loc

    parseSourceToDouble loc =
      let slice = sliceStartCurrent loc scanner
      in
        case readMaybe (BS.unpack slice) of
          Just d  -> Just (L_NUMBER d)
          Nothing -> Nothing

scanForKeywordsAndIdentifiers :: Loc -> Scanner -> (Loc, Scanner)
scanForKeywordsAndIdentifiers loc scanner 
  | isAlphaNum (peek loc scanner) =
        scanForKeywordsAndIdentifiers (fst (advance loc scanner)) scanner

  | otherwise =
      let substr = sliceStartCurrent loc scanner
          tt     = fromMaybe IDENTIFIER (keyword substr)
      in (loc, addToken tt loc scanner)
    

-- scan a single token

scanToken :: Loc -> Scanner -> (Loc, Scanner)
scanToken loc scanner =
  let (loc1, c) = advance loc scanner
      (loc2, scanner1) = case c of
        -- 1 char
        '('  -> (loc1, addToken LEFT_PAREN  loc1 scanner)
        ')'  -> (loc1, addToken RIGHT_PAREN loc1 scanner)
        '{'  -> (loc1, addToken LEFT_BRACE  loc1 scanner)
        '}'  -> (loc1, addToken RIGHT_BRACE loc1 scanner)
        ','  -> (loc1, addToken COMMA       loc1 scanner)
        '.'  -> (loc1, addToken DOT         loc1 scanner)
        '-'  -> (loc1, addToken MINUS       loc1 scanner)
        '+'  -> (loc1, addToken PLUS        loc1 scanner)
        ';'  -> (loc1, addToken SEMICOLON   loc1 scanner)
        '*'  -> (loc1, addToken STAR        loc1 scanner)
        ' '  -> (loc1, scanner)
        '\r' -> (loc1, scanner)
        '\t' -> (loc1, scanner)
        '\n' -> (loc1 { l_line = l_line loc1 + 1 }, scanner)

        -- 1-2 chars
        '!' ->
          let (loc2, matched) = match loc1 scanner '='
          in (loc2, addToken (if matched then BANG_EQUAL else BANG) loc2 scanner)
        '=' ->
          let (loc2, matched) = match loc1 scanner '='
          in (loc2, addToken (if matched then EQUAL_EQUAL else EQUAL) loc2 scanner)
        '<' ->
          let (loc2, matched) = match loc1 scanner '='
          in (loc2, addToken (if matched then LESS_EQUAL else LESS) loc2 scanner)
        '>' ->
          let (loc2, matched) = match loc1 scanner '='
          in (loc2, addToken (if matched then GREATER_EQUAL else GREATER) loc2 scanner)

        -- 1+ chars 
        '/' -> scanForSlashes loc1 scanner
        '"' -> scanForStrings loc1 scanner
        _ | isDigit c -> scanForNumbers loc scanner
        _ | isAlpha c -> scanForKeywordsAndIdentifiers loc scanner

        -- default value
        u_val ->
          let err = createError (l_line loc1) ("Unexpected Character '" ++ [u_val] ++ "'")
          in (loc1, scanner { s_errors = err : s_errors scanner })
  in (loc2, scanner1)
-- scan loop

scanTokens :: Loc -> Scanner -> (Loc, Scanner)
scanTokens loc scanner
  | isAtEnd loc scanner =
      let eofTok   = createToken EOF BS.empty Nothing (l_line loc)
          scanner1 = scanner { s_tokens = eofTok : s_tokens scanner }
      in (loc, scanner1)

  | otherwise =
      let loc1             = loc { l_start = l_current loc }
          (loc2, scanner2) = scanToken loc1 scanner
      in scanTokens loc2 scanner2

-- reverse functions

reverseTokensFromScanner :: Scanner -> Scanner
reverseTokensFromScanner scanner =
  scanner { s_tokens = reverse $ s_tokens scanner }

reverseErrorsFromScanner :: Scanner -> Scanner
reverseErrorsFromScanner scanner =
  scanner { s_errors = reverse $ s_errors scanner }

reverseTokensErrorsFromScanner :: Scanner -> Scanner
reverseTokensErrorsFromScanner scanner =
  scanner
    { s_tokens = reverse $ s_tokens scanner
    , s_errors = reverse $ s_errors scanner
    }

-- pretty print functions

prettyPrintToken :: Token -> IO ()
prettyPrintToken token = do
  putStrLn "Token: "
  putStrLn $ "  type    = " ++ show (t_type token)
  putStrLn $ "  lexeme  = " ++ show (t_lexeme token)
  -- putStrLn $ "  lexeme  = " ++ BS.unpack (t_lexeme token)
  putStrLn $ "  literal = " ++ show (t_literal token)
  putStrLn $ "  line    = " ++ show (t_line token)
  -- putStrLn ""

prettyPrintLoc:: Loc -> IO ()
prettyPrintLoc loc = do
  putStrLn "Loc: "
  putStrLn $ "  start   = " ++ show (l_start loc)
  putStrLn $ "  current = " ++ show (l_current loc)
  putStrLn $ "  line    = " ++ show (l_line loc)

data PpScanOp = ShowTokensOnly | ShowSource | ShowErrors | ShowSourceErrors

prettyPrintScanner :: Scanner -> PpScanOp -> IO ()
prettyPrintScanner scanner ShowTokensOnly = do
  mapM_ prettyPrintToken (s_tokens scanner)

prettyPrintScanner scanner ShowErrors = do
  mapM_ prettyPrintToken (s_tokens scanner)
  mapM_ putStrLn (s_errors scanner)

prettyPrintScanner scanner ShowSource = do
  putStrLn $ "Source: " ++ show (s_source scanner)
  mapM_ prettyPrintToken (s_tokens scanner)

prettyPrintScanner scanner ShowSourceErrors = do
  putStrLn $ "Source: " ++ show (s_source scanner)
  mapM_ prettyPrintToken (s_tokens scanner)
  mapM_ putStrLn (s_errors scanner)

-- main

run :: Loc -> String -> PpScanOp -> IO ()
run loc source op = do
  let scanner          = initScanner source
  let (loc1, scanner1) = scanTokens loc scanner
  let scanner2         = reverseTokensErrorsFromScanner scanner1
  prettyPrintScanner scanner2 op

runFile :: Loc -> String -> IO ()
runFile loc file_path = do
  bytes <- readFile file_path
  run loc bytes ShowSourceErrors

runPrompt :: Loc -> IO ()
runPrompt loc = do
  line <- getLine
  if line == ":q"
    then putStrLn "Quitting..."
    else do
      putStr "> "
      run loc line ShowSourceErrors
      runPrompt loc

main :: IO ()
main = do
  let loc = initLoc
  args <- getArgs
  case args of
    []  -> runPrompt loc
    [p] -> runFile loc p
    _ -> error "Too many args. Can only take one arg"

