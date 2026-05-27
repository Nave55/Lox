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

data Object
  = OBJ_NUMBER Double
  | OBJ_STRING String
  | OBJ_BOOL   Bool
  | OBJ_NIL
  deriving (Show)

data Token = Token
  { t_type    :: TokenType
  , t_lexeme  :: String
  , t_literal :: Object
  , t_line    :: Int
  }
  deriving (Show)

data Scanner = Scanner
  { s_source :: String
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

-- basic functions

subStr :: Int -> Int -> String -> String
subStr start end text = take (end - start) (drop start text)

createError :: Int -> String -> String
createError line = report line ""

report :: Int -> String -> String -> String
report line loc msg = do
  "[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ msg

run :: String -> String
run str = str

runFile :: String -> IO String
runFile = readFile

runPrompt :: IO ()
runPrompt = do
  line <- getLine
  if line == ":q"
    then putStrLn "Quitting..."
    else do
      putStr "> "
      let str = run line
      putStrLn str
      runPrompt

createToken :: TokenType -> String -> Object -> Int -> Token
createToken t_type t_lexeme t_literal t_line =
  Token { t_type, t_lexeme, t_literal, t_line }

toString :: Token -> String
toString token =
  show (t_type token) ++ " " ++ t_lexeme token ++ " " ++ show (t_literal token)

initScanner :: String -> Scanner
initScanner s_source =  Scanner { s_source, s_tokens = [], s_errors = [] }

createScanner :: String -> [Token] -> [String] -> Scanner
createScanner s_source s_tokens s_errors = Scanner { s_source, s_tokens, s_errors }

initLoc :: Loc
initLoc = Loc { l_start = 0, l_current = 0, l_line = 1 }

createLoc :: Int -> Int -> Int -> Loc
createLoc l_start l_current l_line =
  Loc { l_start, l_current, l_line }

createTsl :: Token -> Scanner -> Loc -> Tsl
createTsl tsl_token tsl_scanner tsl_loc =
  Tsl { tsl_token, tsl_scanner, tsl_loc }

-- scanner core

isAtEnd :: Int -> String -> Bool
isAtEnd curr source = curr >= length source

advance :: Loc -> Scanner -> (Loc, Char)
advance loc scanner =
  let n_curr = l_current loc + 1
      loc'   = loc { l_current = n_curr }
      c      = s_source scanner !! (n_curr - 1)
  in (loc', c)

-- addToken variants (like Java: addToken(type) and addToken(type, literal))

addToken :: TokenType -> Loc -> Scanner -> Scanner
addToken t_type = addTokenWithLiteral t_type OBJ_NIL

addTokenWithLiteral :: TokenType -> Object -> Loc -> Scanner -> Scanner
addTokenWithLiteral t_type literal loc scanner =
  let text   = subStr (l_start loc) (l_current loc) (s_source scanner)
      token  = createToken t_type text literal (l_line loc)
  in scanner { s_tokens = token : s_tokens scanner }

-- scan a single token

scanToken :: Loc -> Scanner -> (Loc, Scanner)
scanToken loc scanner =
  let (n_loc, c) = advance loc scanner
      scanner'   = case c of
        '(' -> addToken LEFT_PAREN  n_loc scanner
        ')' -> addToken RIGHT_PAREN n_loc scanner
        '{' -> addToken LEFT_BRACE  n_loc scanner
        '}' -> addToken RIGHT_BRACE n_loc scanner
        ',' -> addToken COMMA       n_loc scanner
        '.' -> addToken DOT         n_loc scanner
        '-' -> addToken MINUS       n_loc scanner
        '+' -> addToken PLUS        n_loc scanner
        ';' -> addToken SEMICOLON   n_loc scanner
        '*' -> addToken STAR        n_loc scanner

        -- DEFAULT CASE (PURE, NO IO)
        u_val ->
          let err = createError (l_line n_loc) ("Unexpected Character '" ++ [u_val] ++ "'")
          in scanner { s_errors = err : s_errors scanner }
  in (n_loc, scanner')

-- scan loop

scanTokens :: Loc -> Scanner -> Tsl
scanTokens loc scanner
  | isAtEnd (l_current loc) (s_source scanner) =
      let eofTok = createToken EOF "" OBJ_NIL (l_line loc)
      in createTsl eofTok scanner loc

  | otherwise =
      let loc'          = loc { l_start = l_current loc }
          (loc'', sc')  = scanToken loc' scanner
      in scanTokens loc'' sc'

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

prettyPrintToken :: Token -> IO ()
prettyPrintToken token = do
  putStrLn "Token: "
  putStrLn $ "  type    = " ++ show (t_type token)
  putStrLn $ "  lexeme  = " ++ show (t_lexeme token)
  putStrLn $ "  literal = " ++ show (t_literal token)
  putStrLn $ "  line    = " ++ show (t_line token)
  -- putStrLn ""

prettyPrintScanner :: Scanner -> IO ()
prettyPrintScanner scanner = do
  putStrLn $ "Source: " ++ s_source scanner
  mapM_ prettyPrintToken (s_tokens scanner)
  mapM_ putStrLn (s_errors scanner)

-- main

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

data Object
  = OBJ_NUMBER Double
  | OBJ_STRING String
  | OBJ_BOOL   Bool
  | OBJ_NIL
  deriving (Show)

data Token = Token
  { t_type    :: TokenType
  , t_lexeme  :: String
  , t_literal :: Object
  , t_line    :: Int
  }
  deriving (Show)

data Scanner = Scanner
  { s_source :: String
  , s_tokens :: [Token]
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

subStr :: Int -> Int -> String -> String
subStr start end text = take (end - start) (drop start text)

printError :: Int -> String -> IO Err
printError line = report line ""

report :: Int -> String -> String -> IO Err
report line loc msg = do
  putStrLn ("[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ msg)
  pure ERR

run :: String -> String
run str = str

runFile :: String -> IO String
runFile = readFile

runPrompt :: IO ()
runPrompt = do
  line <- getLine
  if line == ":q"
    then putStrLn "Quitting..."
    else do
      putStr "> "
      let str = run line
      putStrLn str
      runPrompt

createToken :: TokenType -> String -> Object -> Int -> Token
createToken t_type t_lexeme t_literal t_line =
  Token { t_type, t_lexeme, t_literal, t_line }

toString :: Token -> String
toString token =
  show (t_type token) ++ " " ++ t_lexeme token ++ " " ++ show (t_literal token)

createScanner :: String -> [Token] -> Scanner
createScanner s_source s_tokens = Scanner { s_source, s_tokens }

initLoc :: Loc
initLoc = Loc { l_start = 0, l_current = 0, l_line = 1 }

createLoc :: Int -> Int -> Int -> Loc
createLoc l_start l_current l_line =
  Loc { l_start, l_current, l_line }

createTsl :: Token -> Scanner -> Loc -> Tsl
createTsl tsl_token tsl_scanner tsl_loc =
  Tsl { tsl_token, tsl_scanner, tsl_loc }

-- scanner core

isAtEnd :: Int -> String -> Bool
isAtEnd curr source = curr >= length source

advance :: Loc -> Scanner -> (Loc, Char)
advance loc scanner =
  let n_curr = l_current loc + 1
      loc'   = loc { l_current = n_curr }
      c      = s_source scanner !! (n_curr - 1)
  in (loc', c)

-- addToken variants (like Java: addToken(type) and addToken(type, literal))

addToken :: TokenType -> Loc -> Scanner -> Scanner
addToken t_type = addTokenWithLiteral t_type OBJ_NIL

addTokenWithLiteral :: TokenType -> Object -> Loc -> Scanner -> Scanner
addTokenWithLiteral t_type literal loc scanner =
  let text   = subStr (l_start loc) (l_current loc) (s_source scanner)
      token  = createToken t_type text literal (l_line loc)
  in scanner { s_tokens = token : s_tokens scanner }

-- scan a single token

scanToken :: Loc -> Scanner -> (Loc, Scanner)
scanToken loc scanner =
  let (n_loc, c) = advance loc scanner
      scanner'   = case c of
        '(' -> addToken LEFT_PAREN  n_loc scanner
        ')' -> addToken RIGHT_PAREN n_loc scanner
        '{' -> addToken LEFT_BRACE  n_loc scanner
        '}' -> addToken RIGHT_BRACE n_loc scanner
        ',' -> addToken COMMA       n_loc scanner
        '.' -> addToken DOT         n_loc scanner
        '-' -> addToken MINUS       n_loc scanner
        '+' -> addToken PLUS        n_loc scanner
        ';' -> addToken SEMICOLON   n_loc scanner
        '*' -> addToken STAR        n_loc scanner
        _   -> scanner
  in (n_loc, scanner')

-- scan loop

scanTokens :: Loc -> Scanner -> Tsl
scanTokens loc scanner
  | isAtEnd (l_current loc) (s_source scanner) =
      let eofTok = createToken EOF "" OBJ_NIL (l_line loc)
      in createTsl eofTok scanner loc

  | otherwise =
      let loc'          = loc { l_start = l_current loc }
          (loc'', sc')  = scanToken loc' scanner
      in scanTokens loc'' sc'

reverseTokensFromScanner :: Scanner -> Scanner
reverseTokensFromScanner scanner =
  scanner {s_tokens = reverse $ s_tokens scanner}

reverseTokensFromTsl :: Tsl -> Tsl
reverseTokensFromTsl tsl =
  tsl {tsl_scanner = reverseTokensFromScanner $ tsl_scanner tsl}

prettyPrintToken :: Token -> IO ()
prettyPrintToken token = do
  putStrLn "Token: "
  putStrLn $ "  type    = " ++ show (t_type token)
  putStrLn $ "  lexeme  = " ++ show (t_lexeme token)
  putStrLn $ "  literal = " ++ show (t_literal token)
  putStrLn $ "  line    = " ++ show (t_line token)
  -- putStrLn ""

prettyPrintScanner :: Scanner -> IO ()
prettyPrintScanner scanner = do
  putStrLn $ "Source: " ++ s_source scanner
  mapM_ prettyPrintToken (s_tokens scanner)

-- main

main :: IO ()
main = do
  let loc = initLoc
  args <- getArgs
  case args of
    [] -> runPrompt
    [p] -> do
      result <- runFile p
      let scanner = createScanner result []
      let tsl = reverseTokensFromTsl $ scanTokens loc scanner
      prettyPrintScanner $ tsl_scanner tsl

    _ -> error "Too many args. Can only take one arg"
