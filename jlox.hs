-- Haskell port of the Jlox interpreter from https://craftinginterpreters.com

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

-- string and error functions

createError :: Int -> String -> String
createError line = report line ""

report :: Int -> String -> String -> String
report line loc msg = do
  "[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ msg

sliceBs :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBs i j bs = BS.take (j - i) (BS.drop i bs)

-- *******************************************
--                  SCANNER
-- *******************************************

-- ATD's 

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
  deriving (Show, Eq)

data Literal
  = L_NUMBER Double
  | L_STRING BS.ByteString
  | L_BOOL   Bool
  | L_NIL
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

-- functions to create ADT's

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

-- scanner helper functions

printLiteral :: Literal -> String
printLiteral (L_NUMBER n) = show n
printLiteral (L_STRING s) = BS.unpack s
printLiteral (L_BOOL b)   = if b then "true" else "false"
printLiteral L_NIL        = "nil"

tokenToString :: Token -> String
tokenToString token =
  show (t_type token) ++ " " ++ show (t_lexeme token) ++ " " ++ show (t_literal token)

sliceScannerStartCurrent :: Loc -> Scanner -> BS.ByteString
sliceScannerStartCurrent loc scanner =
  sliceBs (l_start loc) (l_current loc) (s_source scanner)

sliceStartScannerCurrentOff :: Loc -> Scanner -> Int -> Int -> BS.ByteString
sliceStartScannerCurrentOff loc scanner s_off c_off =
  sliceBs (l_start loc + s_off) (l_current loc + c_off) (s_source scanner)

scannerSourceLen :: Scanner -> Int
scannerSourceLen scanner = BS.length $ s_source scanner

charAtScannerCur :: Loc -> Scanner -> Char
charAtScannerCur loc scanner = BS.index (s_source scanner) (l_current loc)

charAtScannerCurOff :: Loc -> Scanner -> Int -> Char
charAtScannerCurOff loc scanner off = BS.index (s_source scanner) (l_current loc + off)

scannerIsAtEnd :: Loc -> Scanner -> Bool
scannerIsAtEnd loc scanner = l_current loc >= scannerSourceLen scanner

scannerAdvance :: Loc -> Scanner -> (Loc, Char)
scannerAdvance loc scanner =
  let c      = charAtScannerCur loc scanner
      n_curr = l_current loc + 1
      loc1   = loc { l_current = n_curr }
  in (loc1, c)

scannerPeek :: Loc -> Scanner -> Char
scannerPeek loc scanner
  | scannerIsAtEnd loc scanner = '\0'
  | otherwise = charAtScannerCur loc scanner

scannerPeekNext :: Loc -> Scanner -> Char
scannerPeekNext loc scanner
  | l_current loc + 1 >= BS.length (s_source scanner) = '\0'
  | otherwise = BS.index (s_source scanner) (l_current loc + 1)

scannerMatch :: Loc -> Scanner -> Char -> (Loc, Bool)
scannerMatch loc scanner expected
  | scannerIsAtEnd loc scanner = (loc, False)
  | charAtScannerCur loc scanner /= expected = (loc, False)
  | otherwise = (loc {l_current = l_current loc + 1}, True)

scannerKeyword :: BS.ByteString -> Maybe TokenType
scannerKeyword "and"    = Just AND
scannerKeyword "class"  = Just CLASS
scannerKeyword "else"   = Just ELSE
scannerKeyword "false"  = Just FALSE
scannerKeyword "fun"    = Just FUN
scannerKeyword "for"    = Just FOR
scannerKeyword "if"     = Just IF
scannerKeyword "nil"    = Just NIL
scannerKeyword "or"     = Just OR
scannerKeyword "print"  = Just PRINT
scannerKeyword "return" = Just RETURN
scannerKeyword "super"  = Just SUPER
scannerKeyword "this"   = Just THIS
scannerKeyword "true"   = Just TRUE
scannerKeyword "var"    = Just VAR
scannerKeyword "while"  = Just WHILE
scannerKeyword _        = Nothing

scannerAddToken :: TokenType -> Loc -> Scanner -> Scanner
scannerAddToken t_type = scannerAddTokenWithLiteral t_type Nothing

scannerAddTokenWithLiteral :: TokenType -> Maybe Literal -> Loc -> Scanner -> Scanner
scannerAddTokenWithLiteral t_type literal loc scanner =
  let text  = sliceScannerStartCurrent loc scanner
      token = createToken t_type text literal (l_line loc)
  in scanner { s_tokens = token : s_tokens scanner }

-- scan functions

scanForSlashes :: Loc -> Scanner -> (Loc, Scanner)
scanForSlashes loc scanner
  | matched   = consumeComment loc1
  | otherwise = (loc, scannerAddTokenWithLiteral SLASH Nothing loc scanner)
  where
    (loc1, matched) = scannerMatch loc scanner '/'

    consumeComment loc =
      let c = scannerPeek loc scanner
      in if c /= '\n' && not (scannerIsAtEnd loc scanner)
           then
             let (loc1, _) = scannerAdvance loc scanner
             in consumeComment loc1
           else
             (loc, scanner)

scanForStrings :: Loc -> Scanner -> (Loc, Scanner)
scanForStrings loc scanner
  | scannerPeek loc scanner /= '"' && not (scannerIsAtEnd loc scanner) =
      let loc1 =
            if scannerPeek loc scanner == '\n'
              then loc { l_line = l_line loc + 1 }
              else loc

          (loc2, _) = scannerAdvance loc1 scanner
      in scanForStrings loc2 scanner

  | scannerIsAtEnd loc scanner =
      let err = createError (l_line loc) "Unterminated String."
      in (loc, scanner { s_errors = err : s_errors scanner })

  | otherwise =
      let (loc1, _) = scannerAdvance loc scanner
          val       = sliceStartScannerCurrentOff loc1 scanner 1 (-1)
          lit       = Just (L_STRING val)
      in (loc1, scannerAddTokenWithLiteral STRING lit loc1 scanner)

scanForNumbers :: Loc -> Scanner -> (Loc, Scanner)
scanForNumbers loc scanner =
  let loc1 = consumeDigits loc

      loc2 =
        if scannerPeek loc1 scanner == '.' && isDigit (scannerPeekNext loc1 scanner)
          then fst (scannerAdvance loc1 scanner)
          else loc1

      loc3 = consumeDigits loc2
  in
    (loc3, scannerAddTokenWithLiteral NUMBER (parseSourceToDouble loc3) loc3 scanner)

  where
    consumeDigits loc
      | isDigit (scannerPeek loc scanner) =
          let (loc1, _) = scannerAdvance loc scanner
          in consumeDigits loc1
      | otherwise = loc

    parseSourceToDouble loc =
      let slice = sliceScannerStartCurrent loc scanner
      in
        case readMaybe (BS.unpack slice) of
          Just d  -> Just (L_NUMBER d)
          Nothing -> Nothing

scanForKeywordsAndIdentifiers :: Loc -> Scanner -> (Loc, Scanner)
scanForKeywordsAndIdentifiers loc scanner
  | isAlphaNum (scannerPeek loc scanner) =
        scanForKeywordsAndIdentifiers (fst (scannerAdvance loc scanner)) scanner

  | otherwise =
      let substr = sliceScannerStartCurrent loc scanner
          tt     = fromMaybe IDENTIFIER (scannerKeyword substr)
      in (loc, scannerAddToken tt loc scanner)


-- scan a single token

scanToken :: Loc -> Scanner -> (Loc, Scanner)
scanToken loc scanner =
  let (loc1, c) = scannerAdvance loc scanner
      (loc2, scanner1) = case c of
        -- 1 char
        '('  -> (loc1, scannerAddToken LEFT_PAREN  loc1 scanner)
        ')'  -> (loc1, scannerAddToken RIGHT_PAREN loc1 scanner)
        '{'  -> (loc1, scannerAddToken LEFT_BRACE  loc1 scanner)
        '}'  -> (loc1, scannerAddToken RIGHT_BRACE loc1 scanner)
        ','  -> (loc1, scannerAddToken COMMA       loc1 scanner)
        '.'  -> (loc1, scannerAddToken DOT         loc1 scanner)
        '-'  -> (loc1, scannerAddToken MINUS       loc1 scanner)
        '+'  -> (loc1, scannerAddToken PLUS        loc1 scanner)
        ';'  -> (loc1, scannerAddToken SEMICOLON   loc1 scanner)
        '*'  -> (loc1, scannerAddToken STAR        loc1 scanner)
        ' '  -> (loc1, scanner)
        '\r' -> (loc1, scanner)
        '\t' -> (loc1, scanner)
        '\n' -> (loc1 { l_line = l_line loc1 + 1 }, scanner)

        -- 1-2 chars
        '!' ->
          let (loc2, matched) = scannerMatch loc1 scanner '='
          in (loc2, scannerAddToken (if matched then BANG_EQUAL else BANG) loc2 scanner)
        '=' ->
          let (loc2, matched) = scannerMatch loc1 scanner '='
          in (loc2, scannerAddToken (if matched then EQUAL_EQUAL else EQUAL) loc2 scanner)
        '<' ->
          let (loc2, matched) = scannerMatch loc1 scanner '='
          in (loc2, scannerAddToken (if matched then LESS_EQUAL else LESS) loc2 scanner)
        '>' ->
          let (loc2, matched) = scannerMatch loc1 scanner '='
          in (loc2, scannerAddToken (if matched then GREATER_EQUAL else GREATER) loc2 scanner)

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
  | scannerIsAtEnd loc scanner =
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

-- *******************************************
--                 Expressions
-- *******************************************
-- expr :: Expr
-- expr =
--   E_BINARY
--     (E_UNARY
--         (Token MINUS "-" Nothing 1)
--         (E_LITERAL (Just (L_NUMBER 123)))
--     )
--     (Token STAR "*" Nothing 1)
--     (E_GROUPING
--         (E_LITERAL (Just (L_NUMBER 45.67)))
--     )

data Expr
  = E_BINARY   Expr Token Expr
  | E_GROUPING Expr
  | E_LITERAL  (Maybe Literal)
  | E_UNARY    Token Expr
  deriving (Show)

printExpr :: Expr -> String
printExpr (E_UNARY token right) =
  "(" ++ BS.unpack (t_lexeme token) ++ " " ++ printExpr right ++ ")"

printExpr (E_BINARY left token right) =
  "(" ++ BS.unpack (t_lexeme token) ++ " " ++ printExpr left ++ " " ++ printExpr right ++ ")"

printExpr (E_GROUPING e) =
  "(group " ++ printExpr e ++ ")"

printExpr (E_LITERAL Nothing)  = "nil"
printExpr (E_LITERAL (Just lit)) = printLiteral lit

-- *******************************************
--                  Parsing
-- *******************************************

data Parser = Parser [Token] Token Token

-- create adt functions

createParser :: [Token] -> Parser
createParser (x:xs) =
  Parser xs x x

-- parser functions

parserPeek :: Parser -> Token
parserPeek (Parser _ _ cur) = cur

parserPrevious :: Parser -> Token
parserPrevious (Parser _ prev _) = prev

parserIsAtEnd :: Parser -> Bool
parserIsAtEnd p = t_type (parserPeek p) == EOF

parserAdvance :: Parser -> Parser
parserAdvance (Parser (t:ts) prev cur) =
  Parser ts cur t

parserCheck :: Parser -> TokenType -> Bool
parserCheck parser tt =
  not (parserIsAtEnd parser) && (t_type (parserPeek parser) == tt)

parserMatch :: [TokenType] -> Parser -> (Bool, Parser)
parserMatch [] p = (False, p)
parserMatch (t:ts) p
  | parserCheck p t = (True, parserAdvance p)
  | otherwise = parserMatch ts p

parserConsume :: TokenType -> String -> Parser -> (Parser, Token)
parserConsume tt msg p
  | parserCheck p tt =
      let p1 = parserAdvance p
      in (p1, parserPrevious p1)
  | otherwise = error msg

parserExpression :: Parser -> (Expr, Parser)
parserExpression = parserEquality

parseMatchRest :: Expr -> Parser -> [TokenType] -> (Parser -> (Expr, Parser)) -> (Expr, Parser)
parseMatchRest expr parser tokens func =
  let (match, parser1) = parserMatch tokens parser
  in if match
     then
       let operator         = parserPrevious parser1
           (right, parser2) = func parser1
           expr1            = E_BINARY expr operator right
       in parseMatchRest expr1 parser2 tokens func
     else
       (expr, parser1)

parserEquality :: Parser -> (Expr, Parser)
parserEquality parser =
  let (expr, parser1) = parserComparison parser
  in parseMatchRest expr parser1 [BANG_EQUAL, EQUAL_EQUAL] parserComparison

parserComparison :: Parser -> (Expr, Parser)
parserComparison parser =
  let (expr, parser1) = parserTerm parser
  in parseMatchRest expr parser1 [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] parserTerm

parserTerm :: Parser -> (Expr, Parser)
parserTerm parser =
  let (expr, parser1) = parserFactor parser
  in parseMatchRest expr parser1 [MINUS, PLUS] parserFactor

parserFactor :: Parser -> (Expr, Parser)
parserFactor parser =
  let (expr, parser1) = parserUnary parser
  in parseMatchRest expr parser1 [SLASH, STAR] parserUnary

parserUnary :: Parser -> (Expr, Parser)
parserUnary parser =
  let (match, parser1) = parserMatch [BANG, MINUS] parser
  in if match
     then
       let operator             = parserPrevious parser1
           (rightExpr, parser2) = parserUnary parser1
       in (E_UNARY operator rightExpr, parser2)
     else
       parserPrimary parser

parserPrimary :: Parser -> (Expr, Parser)
parserPrimary p0 =
  let (mFalse, p1) = parserMatch [FALSE] p0
  in if mFalse
       then (E_LITERAL (Just (L_BOOL False)), p1)
       else

  let (mTrue, p2) = parserMatch [TRUE] p1
  in if mTrue
       then (E_LITERAL (Just (L_BOOL True)), p2)
       else

  let (mNil, p3) = parserMatch [NIL] p2
  in if mNil
       then (E_LITERAL Nothing, p3)
       else

  let (mNumStr, p4) = parserMatch [NUMBER, STRING] p3
  in if mNumStr
       then case t_literal (parserPrevious p4) of
              Just lit -> (E_LITERAL (Just lit), p4)
              Nothing  -> error "Expected literal"
       else

  let (mParen, p5) = parserMatch [LEFT_PAREN] p4
  in if mParen
       then
         let (expr, p6) = parserExpression p5
             (p7, _)    = parserConsume RIGHT_PAREN "Expect ')' after expression." p6
         in (E_GROUPING expr, p7)
       else
         error "Expect expression."

-- *******************************************
--                   MAIN
-- *******************************************

parserRun :: Parser -> [String]
parserRun p = 
  let (expr, p1) = parserExpression p
  in if parserIsAtEnd p1
        then [printExpr expr]
        else printExpr expr : parserRun p1

run :: Loc -> String -> PpScanOp -> IO ()
run loc source op = do
  let scanner          = initScanner source
  let (loc1, scanner1) = scanTokens loc scanner
  let scanner2         = reverseTokensErrorsFromScanner scanner1
  let parser           = createParser (s_tokens scanner2)

  mapM_ putStrLn (parserRun parser)
  -- prettyPrintScanner scanner2 op

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

