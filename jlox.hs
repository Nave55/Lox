module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Bits
import Data.Bifunctor
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>

-- data --

data Err = OK | ERR
  deriving (Show)

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
  | L_STRING String
  | L_BOOL   Bool
  | L_NIL
  deriving (Show)

data Token = Token
  { t_type    :: TokenType
  , t_lexeme  :: BS.ByteString
  , t_literal :: Literal
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

data Tsl = Tsl
  { tsl_token   :: Token
  , tsl_scanner :: Scanner
  , tsl_loc     :: Loc
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

createToken :: TokenType -> BS.ByteString -> Literal -> Int -> Token
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

createTsl :: Token -> Scanner -> Loc -> Tsl
createTsl tsl_token tsl_scanner tsl_loc =
  Tsl { tsl_token, tsl_scanner, tsl_loc }

-- scanner core

sliceBs :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBs i j bs = BS.take (j - i) (BS.drop i bs)

sourceLen :: Scanner -> Int
sourceLen scanner = BS.length $ s_source scanner

isAtEnd :: Loc -> Scanner -> Bool
isAtEnd loc scanner = l_current loc >= sourceLen scanner

advance :: Loc -> Scanner -> (Loc, Char)
advance loc scanner =
  let c      = BS.index (s_source scanner) (l_current loc) 
      n_curr = l_current loc + 1
      loc1   = loc { l_current = n_curr }
  in (loc1, c)

-- addToken variants (like Java: addToken(type) and addToken(type, literal))

addToken :: TokenType -> Loc -> Scanner -> Scanner
addToken t_type = addTokenWithLiteral t_type L_NIL

addTokenWithLiteral :: TokenType -> Literal -> Loc -> Scanner -> Scanner
addTokenWithLiteral t_type literal loc scanner =
  -- let distance = l_current loc - l_start loc
  let text = sliceBs (l_start loc) (l_current loc) (s_source scanner)
      token    = createToken t_type text literal (l_line loc)
  in scanner { s_tokens = token : s_tokens scanner }

match :: Loc -> Scanner -> Char -> (Loc, Bool)
match loc scanner expected 
  | isAtEnd loc scanner = (loc, False)
  | BS.index (s_source scanner) (l_current loc) /= expected = (loc, False)
  | otherwise = (loc {l_current = l_current loc + 1}, True)

-- scan a single token

scanToken :: Loc -> Scanner -> (Loc, Scanner)
scanToken loc scanner =
  let (loc1, c) = advance loc scanner
      (loc2, scanner1) = case c of
        -- single character
        '(' -> (loc1, addToken LEFT_PAREN  loc1 scanner)
        ')' -> (loc1, addToken RIGHT_PAREN loc1 scanner)
        '{' -> (loc1, addToken LEFT_BRACE  loc1 scanner)
        '}' -> (loc1, addToken RIGHT_BRACE loc1 scanner)
        ',' -> (loc1, addToken COMMA       loc1 scanner)
        '.' -> (loc1, addToken DOT         loc1 scanner)
        '-' -> (loc1, addToken MINUS       loc1 scanner)
        '+' -> (loc1, addToken PLUS        loc1 scanner)
        ';' -> (loc1, addToken SEMICOLON   loc1 scanner)
        '*' -> (loc1, addToken STAR        loc1 scanner)

        -- single or double characters
        '!' ->
          let (m_loc, expected) = match loc1 scanner '='
          in (m_loc, addToken (if expected then BANG_EQUAL else BANG) m_loc scanner)
        '=' ->
          let (m_loc, expected) = match loc1 scanner '='
          in (m_loc, addToken (if expected then EQUAL_EQUAL else EQUAL) m_loc scanner)
        '<' ->
          let (m_loc, expected) = match loc1 scanner '='
          in (m_loc, addToken (if expected then LESS_EQUAL else LESS) m_loc scanner)
        '>' ->
          let (m_loc, expected) = match loc1 scanner '='
          in (m_loc, addToken (if expected then GREATER_EQUAL else GREATER) m_loc scanner)

        -- new line
        '\n' ->
          (loc1 { l_line = l_line loc1 + 1 }, scanner)

        -- default value
        u_val ->
          let err = createError (l_line loc1) ("Unexpected Character '" ++ [u_val] ++ "'")
          in (loc1, scanner { s_errors = err : s_errors scanner })
  in (loc2, scanner1)
-- scan loop

scanTokens :: Loc -> Scanner -> Tsl
scanTokens loc scanner
  | isAtEnd loc scanner =
      let eofTok = createToken EOF BS.empty L_NIL (l_line loc)
          scanner1 = scanner { s_tokens = eofTok : s_tokens scanner }
      in createTsl eofTok scanner1 loc

  | otherwise =
      let loc1      = loc { l_start = l_current loc }
          -- scanner1 = scanner { s_lex_start = s_source_rem scanner }
          (loc2, scanner2) = scanToken loc1 scanner
      in scanTokens loc2 scanner2

-- reverse functions

reverseTokensFromScanner :: Scanner -> Scanner
reverseTokensFromScanner scanner =
  scanner { s_tokens = reverse $ s_tokens scanner }

reverseTokensFromTsl :: Tsl -> Tsl
reverseTokensFromTsl tsl =
  tsl { tsl_scanner = reverseTokensFromScanner $ tsl_scanner tsl }

reverseErrorsFromScanner :: Scanner -> Scanner
reverseErrorsFromScanner scanner =
  scanner { s_errors = reverse $ s_errors scanner }

reverseErrorsFromTsl :: Tsl -> Tsl
reverseErrorsFromTsl tsl =
  tsl { tsl_scanner = reverseErrorsFromScanner $ tsl_scanner tsl }

reverseTokensErrorsFromScanner :: Scanner -> Scanner
reverseTokensErrorsFromScanner scanner =
  scanner
    { s_tokens = reverse $ s_tokens scanner
    , s_errors = reverse $ s_errors scanner
    }

reverseTokensErrorsFromTsl :: Tsl -> Tsl
reverseTokensErrorsFromTsl tsl =
  tsl { tsl_scanner = reverseTokensErrorsFromScanner (tsl_scanner tsl) }

-- pretty print functions

prettyPrintToken :: Token -> IO ()
prettyPrintToken token = do
  putStrLn "Token: "
  putStrLn $ "  type    = " ++ show (t_type token)
  putStrLn $ "  lexeme  = " ++ show (t_lexeme token)
  putStrLn $ "  literal = " ++ show (t_literal token)
  putStrLn $ "  line    = " ++ show (t_line token)
  -- putStrLn ""

prettyPrintLoc:: Loc -> IO ()
prettyPrintLoc loc = do
  putStrLn "Loc: "
  putStrLn $ "  start   = " ++ show (l_start loc)
  putStrLn $ "  current = " ++ show (l_current loc)
  putStrLn $ "  line    = " ++ show (l_line loc)

prettyPrintScanner :: Scanner -> IO ()
prettyPrintScanner scanner = do
  putStrLn $ "Source: " ++ show (s_source scanner)
  mapM_ prettyPrintToken (s_tokens scanner)
  mapM_ putStrLn (s_errors scanner)

-- main

-- run :: String -> String
-- run str = str

runFile :: String -> IO String
runFile = readFile

runPrompt :: IO ()
runPrompt = do
  line <- getLine
  if line == ":q"
    then putStrLn "Quitting..."
    else do
      putStr "> "
      -- let str = run line
      putStrLn line
      runPrompt

main :: IO ()
main = do
  let loc = initLoc
  args <- getArgs
  case args of
    [] -> runPrompt
    [p] -> do
      result <- runFile p
      let scanner = initScanner result
      let tsl = reverseTokensErrorsFromTsl $ scanTokens loc scanner
      prettyPrintScanner $ tsl_scanner tsl
  
    _ -> error "Too many args. Can only take one arg"

