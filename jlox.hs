module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Bits
import Data.Bifunctor
import System.Environment (getArgs)

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
  , t_lexeme  :: String
  , t_literal :: Literal
  , t_line    :: Int
  }
  deriving (Show)

data Scanner = Scanner
  { s_source     :: String
  , s_source_rem :: String
  , s_lex_start  :: String
  , s_source_len :: Int
  , s_tokens     :: [Token]
  , s_errors     :: [String]
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

-- basic functions

-- subStr :: Int -> Int -> String -> String
-- subStr start end text = take (end - start) (drop start text)

-- string and error functions

createError :: Int -> String -> String
createError line = report line ""

report :: Int -> String -> String -> String
report line loc msg = do
  "[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ msg

tokenToString :: Token -> String
tokenToString token =
  show (t_type token) ++ " " ++ t_lexeme token ++ " " ++ show (t_literal token)

createToken :: TokenType -> String -> Literal -> Int -> Token
createToken t_type t_lexeme t_literal t_line =
  Token { t_type, t_lexeme, t_literal, t_line }

initScanner :: String -> Scanner
initScanner s_source =
  Scanner
    { s_source     = s_source
    , s_source_rem = s_source
    , s_lex_start  = s_source
    , s_source_len = length s_source
    , s_tokens     = []
    , s_errors     = []
    }

createScanner :: String -> [Token] -> [String] -> Scanner
createScanner s_source s_tokens s_errors =
  Scanner
    { s_source     = s_source
    , s_source_rem = s_source
    , s_lex_start  = s_source
    , s_source_len = length s_source
    , s_tokens     = s_tokens
    , s_errors     = s_errors
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

isAtEnd :: Int -> Int -> Bool
isAtEnd curr len = curr >= len

advance :: Loc -> Scanner -> (Loc, Scanner, Char)
advance loc scanner =
  let c        = head (s_source_rem scanner)
      n_curr   = l_current loc + 1
      loc1     = loc { l_current = n_curr }
      scanner2 = scanner { s_source_rem = tail (s_source_rem scanner) }
  in (loc1, scanner2, c)

-- addToken variants (like Java: addToken(type) and addToken(type, literal))

addToken :: TokenType -> Loc -> Scanner -> Scanner
addToken t_type = addTokenWithLiteral t_type L_NIL

addTokenWithLiteral :: TokenType -> Literal -> Loc -> Scanner -> Scanner
addTokenWithLiteral t_type literal loc scanner =
  let distance = l_current loc - l_start loc
      text     = take distance (s_lex_start scanner)
      token    = createToken t_type text literal (l_line loc)
  in scanner { s_tokens = token : s_tokens scanner }

-- scan a single token

scanToken :: Loc -> Scanner -> (Loc, Scanner)
scanToken loc scanner =
  let (loc1, scanner1, c) = advance loc scanner
      (loc2, scanner2) = case c of
        -- single characters
        '('  -> (loc1, addToken LEFT_PAREN  loc1 scanner1)
        ')'  -> (loc1, addToken RIGHT_PAREN loc1 scanner1)
        '{'  -> (loc1, addToken LEFT_BRACE  loc1 scanner1)
        '}'  -> (loc1, addToken RIGHT_BRACE loc1 scanner1)
        ','  -> (loc1, addToken COMMA       loc1 scanner1)
        '.'  -> (loc1, addToken DOT         loc1 scanner1)
        '-'  -> (loc1, addToken MINUS       loc1 scanner1)
        '+'  -> (loc1, addToken PLUS        loc1 scanner1)
        ';'  -> (loc1, addToken SEMICOLON   loc1 scanner1)
        '*'  -> (loc1, addToken STAR        loc1 scanner1)

        -- new line
        '\n' -> (loc1 { l_line = l_line loc1 + 1 }, scanner1)

        -- default value
        u_val ->
          let err = createError (l_line loc1) ("Unexpected Character '" ++ [u_val] ++ "'")
          in (loc1, scanner1 { s_errors = err : s_errors scanner1 })
  in (loc2, scanner2)

-- scan loop

scanTokens :: Loc -> Scanner -> Tsl
scanTokens loc scanner
  | isAtEnd (l_current loc) (s_source_len scanner) =
      let eofTok = createToken EOF "" L_NIL (l_line loc)
          scanner1 = scanner { s_tokens = eofTok : s_tokens scanner }
      in createTsl eofTok scanner1 loc

  | otherwise =
      let loc1      = loc { l_start = l_current loc }
          scanner1 = scanner { s_lex_start = s_source_rem scanner }
          (loc2, scanner2) = scanToken loc1 scanner1
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
  putStrLn $ "Source:    " ++ s_source scanner
  -- putStrLn $ "Remainder:    " ++ s_source_rem scanner
  putStrLn $ "Lex Start: " ++ s_lex_start scanner
  putStrLn $ "Length:    " ++ show (s_source_len scanner)
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

