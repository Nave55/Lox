{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Haskell port of the Jlox interpreter from https://craftinginterpreters.com

import Data.List
import Debug.Trace
import Numeric (showFFloat)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (isDigit, isAlpha, isAlphaNum)

import qualified Data.Map.Strict       as SM
import qualified Data.ByteString.Char8 as BS

---------------------------------------------------------------------------
--                             Type Aliases
---------------------------------------------------------------------------

type EitherLit             = Either BS.ByteString LoxValue
type EitherEnv             = Either BS.ByteString Env
type EitherBsEnv           = Either BS.ByteString (BS.ByteString, Env)
type EitherLitEnv          = Either BS.ByteString (LoxValue, Env)
type EitherListBsEnv       = Either BS.ByteString ([BS.ByteString], Env)
type EitherParserTok       = Either BS.ByteString (Parser, Token)
type EitherStmtParser      = Either BS.ByteString (Stmt, Parser)
type EitherExprParser      = Either BS.ByteString (Expr, Parser)
type EitherLoxCallable     = Either BS.ByteString LoxCallable
type EitherListBsExecRes   = Either BS.ByteString ([BS.ByteString], ExecResult)
type EitherListStmtParser  = Either BS.ByteString ([Stmt], Parser)
type EitherListBsLitInterp = Either BS.ByteString ([BS.ByteString], LoxValue, Interpreter)

---------------------------------------------------------------------------
--                            Helper Functions
---------------------------------------------------------------------------

loxError :: Int -> BS.ByteString -> BS.ByteString
loxError line = loxReport line ""

loxReport :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
loxReport line loc msg =
  "[line "
    <> BS.pack (show line)
    <> "] Error "
    <> loc
    <> "-> "
    <> msg

sliceBs :: Int -> Int -> BS.ByteString -> BS.ByteString
sliceBs i j bs = BS.take (j - i) (BS.drop i bs)

bsToDouble :: BS.ByteString -> Maybe Double
bsToDouble bs =
  case BS.readInteger bs of
    Just (intPart, rest)
      | BS.null rest ->
          Just (fromInteger intPart)

      | BS.head rest == '.' ->
          case BS.readInteger (BS.tail rest) of
            Just (fracPart, rest2)
              | BS.null rest2 ->
                  let digits = BS.length (BS.tail rest)
                      frac = fromInteger fracPart / (10 ^ digits)
                  in Just (fromInteger intPart + frac)
            _ -> Nothing

    _ -> Nothing

formatNum :: Double -> Integer -> BS.ByteString
formatNum n l =
  BS.pack (stripDotZero (showFFloat Nothing (round' n l) ""))

  where
    stripDotZero s =
      case break (== '.') s of
        (int, "") -> int
        (int, '.':frac) ->
          if all (== '0') frac
            then int
            else int ++ "." ++ dropWhileEnd (== '0') frac

    round' num sg = (fromIntegral . round $ num * f) / f
        where !f = 10^sg

----------------------------------------------------------------------
--                              SCANNER
----------------------------------------------------------------------

data TokenType
  = -- Single-character tokens
    LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
  | COMMA      | DOT         | MINUS      | PLUS
  | SEMICOLON  | SLASH       | STAR

  -- Double-character tokens
  | BANG       | BANG_EQUAL    | EQUAL | EQUAL_EQUAL
  | GREATER    | GREATER_EQUAL | LESS  | LESS_EQUAL
  | PLUS_EQUAL | MINUS_EQUAL

  -- Literals
  | IDENTIFIER | STRING | NUMBER

  -- Keywords
  | AND   | CLASS | ELSE   | FALSE
  | FUNC  | FOR   | IF     | NIL
  | OR    | PRINT | RETURN | SUPER
  | THIS  | TRUE  | VAR    | WHILE
  | BREAK | CONTINUE

  | EOF
  deriving (Show, Eq)

data LoxValue
  = L_NUMBER Double
  | L_STRING BS.ByteString
  | L_BOOL   Bool
  | L_NIL
  | L_CALL  LoxCallable
  | L_FUNC  LoxFunction
  | L_CLASS LoxClass
  | L_INSTANCE LoxInstance

instance Show LoxValue where
  show (L_NUMBER d)      = show d
  show (L_STRING s)      = show s
  show (L_BOOL b)        = show b
  show L_NIL             = "nil"
  show (L_FUNC x)        = "<fn>"
  show (L_CALL x)        = "<native fn>"
  show (L_CLASS c)       = "<class " ++ BS.unpack (lc_name c) ++ ">"
  show (L_INSTANCE inst) =
    "<instance " ++ BS.unpack (lc_name (li_class inst)) ++ ">"

instance Eq LoxValue where
  L_NIL == L_NIL = True
  L_BOOL a == L_BOOL b = a == b
  L_STRING a == L_STRING b = a == b

  L_NUMBER a == L_NUMBER b
    | isNaN a && isNaN b = True
    | otherwise          = a == b

  _ == _ = False

data Token = Token
  { t_type    :: TokenType
  , t_lexeme  :: BS.ByteString
  , t_literal :: Maybe LoxValue
  , t_line    :: Int
  }
  deriving (Show)

data Loc = Loc
  { l_start :: Int
  , l_curr  :: Int
  , l_line  :: Int
  }
  deriving (Show)

data Scanner = Scanner
  { s_source :: BS.ByteString
  , s_tokens :: [Token]
  , s_loc    :: Loc
  , s_errors :: [BS.ByteString]
  }
  deriving (Show)

createToken
  :: TokenType
  -> BS.ByteString
  -> Maybe LoxValue
  -> Int
  -> Token

createToken t_type t_lexeme t_literal t_line =
  Token { t_type, t_lexeme, t_literal, t_line }

initLoc :: Int -> Loc
initLoc l = Loc { l_start = 0, l_curr = 0, l_line = l }

initScanner :: String -> Int -> Scanner
initScanner s_source line =
  Scanner
    { s_source = BS.pack s_source
    , s_tokens = []
    , s_loc    = initLoc line
    , s_errors = []
    }

showLiteral :: LoxValue -> BS.ByteString
showLiteral (L_NUMBER n) = formatNum n 10
showLiteral (L_STRING s) = s
showLiteral (L_BOOL b)   = if b then "true" else "false"
showLiteral L_NIL        = "nil"
showLiteral (L_FUNC _)   = "<fn>"
showLiteral (L_CALL _)   = "<native fn>"
showLiteral (L_CLASS klass)  = lc_name klass
showLiteral (L_INSTANCE inst) = BS.pack (show inst)

data LocField = LocCurr | LocLine
updateLoc :: (Int -> Int) -> Scanner -> LocField -> Scanner
updateLoc f sc LocCurr =
  let loc = s_loc sc
  in sc { s_loc = loc { l_curr = f (l_curr loc) } }

updateLoc f sc LocLine =
  let loc = s_loc sc
  in sc { s_loc = loc { l_line = f (l_line loc) } }

setLocStart :: Scanner -> Scanner
setLocStart sc =
  let loc = s_loc sc
  in sc { s_loc = loc { l_start = l_curr loc } }

-- basic scanner functions

sliceScannerStartCurrent :: Scanner -> BS.ByteString
sliceScannerStartCurrent sc =
  sliceBs (l_start loc) (l_curr loc) (s_source sc)
  where loc = s_loc sc

sliceStartScannerCurrentOff :: Scanner -> Int -> Int -> BS.ByteString
sliceStartScannerCurrentOff sc s_off c_off =
  sliceBs (l_start loc + s_off) (l_curr loc + c_off) (s_source sc)
  where loc = s_loc sc

scannerSourceLen :: Scanner -> Int
scannerSourceLen sc = BS.length $ s_source sc

charAtScannerCur :: Scanner -> Char
charAtScannerCur sc =
  BS.index (s_source sc) (l_curr $ s_loc sc)

charAtScannerCurOff :: Scanner -> Int -> Char
charAtScannerCurOff sc off =
  BS.index (s_source sc) (l_curr (s_loc sc) + off)

scannerIsAtEnd :: Scanner -> Bool
scannerIsAtEnd sc = l_curr (s_loc sc) >= scannerSourceLen sc

scannerAdvance :: Scanner -> (Scanner, Char)
scannerAdvance sc0 =
  let c   = charAtScannerCur sc0
      sc1 = updateLoc (+1) sc0 LocCurr
  in (sc1, c)

scannerPeek :: Scanner -> Char
scannerPeek sc
  | scannerIsAtEnd sc = '\0'
  | otherwise = charAtScannerCur sc

scannerPeekNext :: Scanner -> Char
scannerPeekNext sc
  | n_loc >= BS.length (s_source sc) = '\0'
  | otherwise = BS.index (s_source sc) n_loc

  where
    n_loc = l_curr (s_loc sc) + 1

scannerMatch :: Scanner -> Char -> (Scanner, Bool)
scannerMatch sc expected
  | scannerIsAtEnd sc = (sc, False)
  | charAtScannerCur sc /= expected = (sc, False)
  | otherwise = (updateLoc (+1) sc LocCurr, True)

scannerAddTokenWithLiteral
  :: TokenType
  -> Maybe LoxValue
  -> Scanner
  -> Scanner

scannerAddTokenWithLiteral t_type literal sc =
  let text  = sliceScannerStartCurrent sc
      token = createToken t_type text literal (l_line $ s_loc sc)
  in sc { s_tokens = token : s_tokens sc }

scannerAddToken :: TokenType -> Scanner -> Scanner
scannerAddToken t_type =
  scannerAddTokenWithLiteral t_type Nothing

-- scan functions

scanForSlashes :: Scanner -> Scanner
scanForSlashes sc0 =
  let (sc1, matched) = scannerMatch sc0 '/'
  in if matched
       then consumeComment sc1
       else scannerAddTokenWithLiteral SLASH Nothing sc0
  where
    consumeComment sc =
      let c = scannerPeek sc
      in if c /= '\n' && not (scannerIsAtEnd sc)
           then let (sc1, _) = scannerAdvance sc
                in consumeComment sc1
           else sc

scanForStrings :: Scanner -> Scanner
scanForStrings sc0
  | scannerPeek sc0 /= '"' && not (scannerIsAtEnd sc0) =
      let sc1 =
            if scannerPeek sc0 == '\n'
              then updateLoc (+1) sc0 LocLine
              else sc0
          (sc2, _) = scannerAdvance sc1
      in scanForStrings sc2

  | scannerIsAtEnd sc0 =
      let err = loxError (l_line (s_loc sc0)) "Unterminated String."
      in sc0 { s_errors = err : s_errors sc0 }

  | otherwise =
      let (sc1, _) = scannerAdvance sc0
          val      = sliceStartScannerCurrentOff sc1 1 (-1)
          lit      = Just (L_STRING val)
      in scannerAddTokenWithLiteral STRING lit sc1

scanForNumbers :: Scanner -> Scanner
scanForNumbers sc0 =
  let sc1 = consumeDigits sc0

      sc2 =
        if scannerPeek sc1 == '.' && isDigit (scannerPeekNext sc1)
          then fst (scannerAdvance sc1)
          else sc1

      sc3 = consumeDigits sc2
  in
    scannerAddTokenWithLiteral NUMBER (parseSourceToDouble sc3) sc3

  where
    consumeDigits sc
      | isDigit (scannerPeek sc) =
          let (sc', _) = scannerAdvance sc
          in consumeDigits sc'
      | otherwise = sc

    parseSourceToDouble sc =
      let slice = sliceScannerStartCurrent sc
      in
        case bsToDouble slice of
          Just d  -> Just (L_NUMBER d)
          Nothing -> Nothing

scanForKeywordsAndIdentifiers :: Scanner -> Scanner
scanForKeywordsAndIdentifiers sc
  | isAlphaNum curr || curr == '_' =
        scanForKeywordsAndIdentifiers (fst (scannerAdvance sc))

  | otherwise =
      let substr = sliceScannerStartCurrent sc
          tt     = fromMaybe IDENTIFIER (keyword substr)
      in scannerAddToken tt sc

  where
    curr = scannerPeek sc

    keyword "and"      = Just AND
    keyword "class"    = Just CLASS
    keyword "else"     = Just ELSE
    keyword "false"    = Just FALSE
    keyword "func"     = Just FUNC
    keyword "for"      = Just FOR
    keyword "if"       = Just IF
    keyword "nil"      = Just NIL
    keyword "or"       = Just OR
    keyword "print"    = Just PRINT
    keyword "return"   = Just RETURN
    keyword "super"    = Just SUPER
    keyword "this"     = Just THIS
    keyword "true"     = Just TRUE
    keyword "var"      = Just VAR
    keyword "while"    = Just WHILE
    keyword "break"    = Just BREAK
    keyword "continue" = Just CONTINUE
    keyword _          = Nothing

-- scan a single token

scanToken :: Scanner -> Scanner
scanToken sc0 =
  let (sc1, c) = scannerAdvance sc0
      sc2 = case c of
        -- 1 char
        '('  -> scannerAddToken LEFT_PAREN  sc1
        ')'  -> scannerAddToken RIGHT_PAREN sc1
        '{'  -> scannerAddToken LEFT_BRACE  sc1
        '}'  -> scannerAddToken RIGHT_BRACE sc1
        ','  -> scannerAddToken COMMA       sc1
        '.'  -> scannerAddToken DOT         sc1
        ';'  -> scannerAddToken SEMICOLON   sc1
        '*'  -> scannerAddToken STAR        sc1
        ' '  -> sc1
        '\r' -> sc1
        '\t' -> sc1
        '\n' -> updateLoc (+1) sc1 LocLine

        -- 1-2 chars
        '+'  ->
          let (sc2, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then PLUS_EQUAL else PLUS) sc2
        '-'  ->
          let (sc2, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then MINUS_EQUAL else MINUS) sc2

        '!' ->
          let (sc2, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then BANG_EQUAL else BANG) sc2
        '=' ->
          let (sc2, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then EQUAL_EQUAL else EQUAL) sc2
        '<' ->
          let (sc2, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then LESS_EQUAL else LESS) sc2
        '>' ->
          let (sc2, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then GREATER_EQUAL else GREATER) sc2

        -- 1+ chars 
        '/' -> scanForSlashes sc1
        '"' -> scanForStrings sc1
        _ | isDigit c -> scanForNumbers sc1
        _ | isAlpha c || c == '_' -> scanForKeywordsAndIdentifiers sc1

        -- default value
        u_val ->
          let line = l_line $ s_loc sc1
              err =
                loxError
                  line
                  ( "Unexpected Character '"
                  <> BS.singleton u_val
                  <> "'"
                  )

          in sc1 { s_errors = err : s_errors sc1 }
  in sc2

scanTokens :: Scanner -> Scanner
scanTokens sc0
  | scannerIsAtEnd sc0 =
      let eofTok   = createToken EOF BS.empty Nothing (l_line $ s_loc sc0)
          sc1 = sc0 { s_tokens = eofTok : s_tokens sc0 }
      in check $ reverseTokensErrorsFromScanner sc1

  | otherwise =
      let sc1 = setLocStart sc0
          sc2 = scanToken sc1
      in scanTokens sc2

  where
    initToken = createToken EOF "" Nothing 1
    check sc' = checkBraces (s_tokens sc') sc' (initToken, 0) (initToken, 0)
    -- checks for even amt of braces and parens
    checkBraces :: [Token] -> Scanner -> (Token, Int) -> (Token, Int) -> Scanner
    checkBraces [] sc' (p_tok, p_count) (b_tok, b_count) =
      let p_err =
            loxError
              (t_line p_tok)
              ("Missing accompanying paren for " <> t_lexeme p_tok)

          b_err =
            loxError
              (t_line b_tok)
              ("Missing accompanying brace for " <> t_lexeme b_tok)
      in case (p_count /= 0, b_count /= 0) of
          (True, True)  -> sc' { s_errors = b_err : p_err : s_errors sc' }
          (True, False) -> sc' { s_errors = p_err : s_errors sc' }
          (False, True) -> sc' { s_errors = b_err : s_errors sc' }
          _             -> sc'

    checkBraces (x:xs) sc' (p_tok, p_count) (b_tok, b_count) =
      case t_lexeme x of

        "(" ->
          let newCount = p_count + 1
              newTok   = if p_count == 0 then x else p_tok
          in checkBraces xs sc' (newTok, newCount) (b_tok, b_count)

        ")" ->
          let newCount = p_count - 1
              newTok   = if newCount < 0 then x else p_tok
          in checkBraces xs sc' (newTok, newCount) (b_tok, b_count)

        "{" ->
          let newCount = b_count + 1
              newTok   = if b_count == 0 then x else b_tok
          in checkBraces xs sc' (p_tok, p_count) (newTok, newCount)

        "}" ->
          let newCount = b_count - 1
              newTok   = if newCount < 0 then x else b_tok
          in checkBraces xs sc' (p_tok, p_count) (newTok, newCount)

        _ ->
          checkBraces xs sc' (p_tok, p_count) (b_tok, b_count)

scannerRun :: Scanner -> Scanner
scannerRun = scanTokens

reverseTokensErrorsFromScanner :: Scanner -> Scanner
reverseTokensErrorsFromScanner sc =
  sc
    { s_tokens = reverse $ s_tokens sc
    , s_errors = reverse $ s_errors sc
    }

prettyPrintToken :: Token -> IO ()
prettyPrintToken token = do
  putStrLn "Token: "
  putStrLn $ "  type    = " ++ show (t_type token)
  putStrLn $ "  lexeme  = " ++ show (t_lexeme token)
  putStrLn $ "  literal = " ++ show (t_literal token)
  putStrLn $ "  line    = " ++ show (t_line token)

data PpScanOp
  = PpErrors
  | PpTokens
  | PpTokensSource
  | PpTokensErrors
  | PpAll

prettyPrintScanner :: Scanner -> PpScanOp -> IO ()
prettyPrintScanner sc PpTokens = do
  mapM_ prettyPrintToken (s_tokens sc)

prettyPrintScanner sc PpErrors = do
  mapM_ BS.putStrLn (s_errors sc)

prettyPrintScanner sc PpTokensErrors = do
  mapM_ prettyPrintToken (s_tokens sc)
  mapM_ BS.putStrLn (s_errors sc)

prettyPrintScanner sc PpTokensSource = do
  putStrLn $ "Source: " ++ show (s_source sc)
  mapM_ prettyPrintToken (s_tokens sc)

prettyPrintScanner sc PpAll = do
  putStrLn $ "Source: " ++ show (s_source sc)
  mapM_ prettyPrintToken (s_tokens sc)
  mapM_ BS.putStrLn (s_errors sc)

--------------------------------------------------------------------------------
--                                 Expressions
--------------------------------------------------------------------------------

data Expr
  = E_ASSIGN   Token Expr          -- name value
  | E_BINARY   Expr Token Expr     -- left operator right
  | E_LITERAL  (Maybe LoxValue)    -- value
  | E_LOGICAL  Expr Token Expr     -- left operator right
  | E_UNARY    Token Expr          -- operator right
  | E_CALL     Expr Token [Expr]   -- callee paren args
  | E_GET      Expr Token          -- object name
  | E_SET      Expr Token Expr     -- object name value 
  | E_SUPER    Token Token         -- keyword method
  | E_THIS     Token               -- keyword
  | E_GROUPING Expr                -- expression
  | E_VARIABLE Token               -- name
  deriving (Show)

-----------------------------------------------------------------------------
--                               Parser
-----------------------------------------------------------------------------

-- Parser Tokens Prev Curr
data Parser = Parser [Token] Token Token
  deriving (Show)

createParser (x:xs) =
  let dummy = Token EOF "" Nothing (t_line x)
  in Parser xs dummy x

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
  not (parserIsAtEnd parser)
  && (t_type (parserPeek parser) == tt)

parserMatch :: [TokenType] -> Parser -> (Bool, Parser)
parserMatch [] p = (False, p)
parserMatch (t:ts) p
  | parserCheck p t = (True, parserAdvance p)
  | otherwise       = parserMatch ts p

parserConsume :: TokenType -> BS.ByteString -> Parser -> EitherParserTok
parserConsume tt msg p
  | parserCheck p tt =
      let p1 = parserAdvance p
      in Right (p1, parserPrevious p1)
  | otherwise =
      let c = parserPeek p
      in Left (loxError (t_line c) msg)

synchronize :: Parser -> Parser
synchronize = go . parserAdvance
  where
    go parser
      | parserIsAtEnd parser = parser
      | t_type (parserPrevious parser) == SEMICOLON = parser
      | t_type (parserPeek parser) `elem` syncStarters = parser
      | otherwise = go (parserAdvance parser)

    syncStarters =
      [ CLASS, FUNC, VAR, FOR, IF, WHILE, PRINT, RETURN ]

data BinaryOrLogical = Binary | Logical
  deriving (Eq)

parserMatchRest
  :: Expr
  -> Parser
  -> [TokenType]
  -> (Parser -> EitherExprParser)
  -> BinaryOrLogical
  -> EitherExprParser

parserMatchRest expr parser tokens func bl =
  let (matched, parser1) = parserMatch tokens parser
  in if matched
     then do
       let operator = parserPrevious parser1
       (right, parser2) <- func parser1

       let expr1 =
            if bl == Binary
              then E_BINARY expr operator right
              else E_LOGICAL expr operator right

       parserMatchRest expr1 parser2 tokens func bl

     else
       Right (expr, parser1)

----------------------------------------------------------------------------------------
--                                  Environments
----------------------------------------------------------------------------------------

data Env = Env
  { e_map        :: SM.Map BS.ByteString LoxValue
  , e_enclosing  :: Maybe Env
  , e_loop_depth :: Int
  }
  deriving (Show)

-- 
initEnv :: Env
initEnv = Env
  { e_map        = SM.empty
  , e_enclosing  = Nothing
  , e_loop_depth = 0
  }

newEnv :: Env -> Env
newEnv parent =
  Env
    { e_map        = SM.empty
    , e_enclosing  = Just parent
    , e_loop_depth = e_loop_depth parent
    }

enterLoop :: Env -> Env
enterLoop env = env { e_loop_depth = e_loop_depth env + 1 }

exitLoop :: Env -> Env
exitLoop env = env { e_loop_depth = e_loop_depth env - 1 }

envDefine :: Env -> BS.ByteString -> LoxValue -> Env
envDefine env key val =
  env { e_map = SM.insert key val (e_map env) }

envAssign :: Env -> Token -> LoxValue -> EitherEnv
envAssign env name val =
  let key  = t_lexeme name
      line = t_line name
  in case SM.lookup key (e_map env) of
       Just _ ->
         Right env { e_map = SM.insert key val (e_map env) }

       Nothing ->
         case e_enclosing env of
           Just parent ->
             case envAssign parent name val of
               Right parent' ->
                 Right env { e_enclosing = Just parent' }
               Left e -> Left e

           Nothing ->
             Left (loxError line ("Undefined variable '" <> key <> "'."))

envGet :: Env -> Token -> EitherLit
envGet env token =
  let key  = t_lexeme token
      line = t_line token
  in case SM.lookup key (e_map env) of
       Just v  -> Right v
       Nothing ->
         case e_enclosing env of
           Just parent -> envGet parent token
           Nothing ->
             Left (loxError line ("Undefined variable '" <> key <> "'."))

-------------------------------------------------------------------------------------------
--                                   Interpreter 
-------------------------------------------------------------------------------------------

data Interpreter = Interpreter
  { i_globals     :: Env
  , i_environment :: Env
  }
  deriving (Show)

data LoxCallable = LoxCallable
  { lc_arity :: Int
  , lc_call
      :: Interpreter
      -> Token
      -> [LoxValue]
      -> EitherListBsLitInterp
  }

data LoxFunction = LoxFunction
  { lf_name        :: Token
  , lf_params      :: [Token]
  , lf_body        :: [Stmt]
  , lf_closure     :: Env
  , lf_initializer :: Bool
  }
  deriving (Show)

initInterpreter :: Interpreter
initInterpreter =
  let
    g0 = initEnv
    g1 = foldl' (\env (name, fn) ->
                  envDefine env name (L_CALL fn))
                g0
                builtIns
  in
    Interpreter
      { i_globals     = g1
      , i_environment = g1
      }

newLoxFunction :: Token -> [Token] -> [Stmt] -> Env -> Bool -> LoxFunction
newLoxFunction name params body closure isInit =
  LoxFunction { lf_name = name
              , lf_params = params
              , lf_body = body
              , lf_closure = closure
              , lf_initializer = isInit
              }

callUserFunction fn interp0 tok args = do
  let callerEnv = i_environment interp0
      closure   = lf_closure fn
      params    = lf_params fn
      body      = lf_body fn
      isInit    = lf_initializer fn
      callEnv0  = newEnv closure
      callEnv1  =
        foldl'
          (\env (paramTok, argVal) ->
             envDefine env (t_lexeme paramTok) argVal)
          callEnv0
          (zip params args)

      interp1 = interp0 { i_environment = callEnv1 }

  (outs, res) <- execBlock interp1 [] body

  let restore interpAfter =
        interpAfter { i_environment = callerEnv }

      getThisFrom interpAfter =
        case envGet (i_environment interpAfter)
                    (Token IDENTIFIER "this" Nothing 0) of
          Right lit -> lit
          _         -> L_NIL

  case res of
    ER_RETURN retVal interpAfter ->
      let interpFinal = restore interpAfter in
      if isInit
        then Right (outs, getThisFrom interpAfter, interpFinal)
        else Right (outs, retVal, interpFinal)

    ER_NORMAL interpAfter ->
      let interpFinal = restore interpAfter in
      if isInit
        then Right (outs, getThisFrom interpAfter, interpFinal)
        else Right (outs, L_NIL, interpFinal)

    ER_BREAK _    -> Left (loxError 0 "break outside loop in function body")
    ER_CONTINUE _ -> Left (loxError 0 "continue outside loop in function body")

wrapUserFunction :: LoxFunction -> LoxCallable
wrapUserFunction fn =
  LoxCallable
    { lc_arity = length (lf_params fn)
    , lc_call  = callUserFunction fn
    }

expectCallable :: LoxValue -> EitherLoxCallable
expectCallable (L_CALL c) = Right c
expectCallable (L_FUNC f) = Right (wrapUserFunction f)
expectCallable (L_CLASS k) = Right (classCallable k)
expectCallable _          = Left "Can only call functions and classes."

--------------------------------------------------------------------------------------
--                               Expression parsing 
--------------------------------------------------------------------------------------

parseExpression :: Parser -> EitherExprParser
parseExpression = parseAssignment

parseAssignment :: Parser -> EitherExprParser
parseAssignment p0 = do
  (expr, p1) <- parseOr p0

  let (matched, p2) =
        parserMatch
          [EQUAL, PLUS_EQUAL, MINUS_EQUAL]
          p1

  if matched then do
    let opTok = parserPrevious p2
        op    = t_type opTok

        plusTok  = opTok { t_type = PLUS  }
        minusTok = opTok { t_type = MINUS }

    (value, p3) <- parseAssignment p2

    case expr of
      E_VARIABLE tok ->
        case op of
          EQUAL ->
            Right (E_ASSIGN tok value, p3)

          PLUS_EQUAL ->
            Right (E_ASSIGN tok (E_BINARY expr plusTok value), p3)

          MINUS_EQUAL ->
            Right (E_ASSIGN tok (E_BINARY expr minusTok value), p3)

          _ ->
            Left (loxError (t_line opTok) "Unknown assignment operator")

      E_GET obj nameTok ->
        Right (E_SET obj nameTok value, p3)

      _ ->
        Left (loxError (t_line opTok) "Invalid assignment target")

  else
    Right (expr, p1)

parseEquality :: Parser -> EitherExprParser
parseEquality p0 = do
  (expr, p1) <- parseComparison p0
  parserMatchRest expr p1 [BANG_EQUAL, EQUAL_EQUAL] parseComparison Binary

parseComparison :: Parser -> EitherExprParser
parseComparison p0 = do
  (expr, p1) <- parseTerm p0
  parserMatchRest
    expr
    p1
    [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]
    parseTerm
    Binary

parseTerm :: Parser -> EitherExprParser
parseTerm p0 = do
  (expr, p1) <- parseFactor p0
  parserMatchRest
    expr
    p1
    [MINUS, PLUS]
    parseFactor
    Binary

parseFactor :: Parser -> EitherExprParser
parseFactor p0 = do
  (expr, p1) <- parseUnary p0
  parserMatchRest
    expr
    p1
    [SLASH, STAR]
    parseUnary
    Binary

parseAnd :: Parser -> EitherExprParser
parseAnd p0 = do
  (expr, p1) <- parseEquality p0
  parserMatchRest
    expr
    p1
    [AND]
    parseEquality
    Logical

parseOr :: Parser -> EitherExprParser
parseOr p0 = do
  (expr, p1) <- parseAnd p0
  parserMatchRest
    expr
    p1
    [OR]
    parseAnd
    Logical

parseCall :: Parser -> EitherExprParser
parseCall p0 = do
  (expr0, p1) <- parsePrimary p0
  loop expr0 p1
  where
    loop e0 p0 = do
      case parserMatch [LEFT_PAREN] p0 of
        (True, p1) -> do
          (e1, p2) <- finishCall e0 p1
          loop e1 p2

        (False, _) ->
          case parserMatch [DOT] p0 of
            (True, p1') -> do
              (p2, nameTok) <-
                parserConsume
                  IDENTIFIER
                  "Expect property name after '.'."
                  p1'
              loop (E_GET e0 nameTok) p2

            (False, _) ->
              Right (e0, p0)

    finishCall callee p0 = do
      (p1, args)  <- fcLoop p0 0 []
      (p2, paren) <-
        parserConsume
          RIGHT_PAREN
          "Expect ')' after arguments."
          p1

      Right (E_CALL callee paren (reverse args), p2)

    fcLoop p0 len acc
      | not (parserCheck p0 RIGHT_PAREN) =
          if len >= 255
            then
              Left $
                loxError
                  (t_line $ parserPeek p0)
                  "Can't have more than 255 arguments."

            else do
              (expr, p1) <- parseExpression p0
              let (m, p2) = parserMatch [COMMA] p1
              if m
                then fcLoop p2 (len + 1) (expr : acc)
                else Right (p2, expr : acc)

      | otherwise
          = Right (p0, reverse acc)

parseUnary :: Parser -> EitherExprParser
parseUnary p0 =
  let (matched, p1) = parserMatch [BANG, MINUS] p0
  in if matched
     then do
       let operator = parserPrevious p1
       (rightExpr, p2) <- parseUnary p1
       Right (E_UNARY operator rightExpr, p2)
     else
       parseCall p0

parsePrimary :: Parser -> EitherExprParser
parsePrimary p0
  | (True, p1) <- parserMatch [FALSE] p0
      = Right (E_LITERAL (Just (L_BOOL False)), p1)

  | (True, p1) <- parserMatch [TRUE] p0
      = Right (E_LITERAL (Just (L_BOOL True)), p1)

  | (True, p1) <- parserMatch [NIL] p0
      = Right (E_LITERAL Nothing, p1)

  | (True, p1) <- parserMatch [NUMBER, STRING] p0
      = case t_literal (parserPrevious p1) of
        Just lit -> Right (E_LITERAL (Just lit), p1)
        Nothing  -> Left "Expected literal"

  | (True, p1) <- parserMatch [SUPER] p0
      = do
          (p2, _) <- parserConsume DOT "Expect '.' after 'super'." p1
          (p3, methodTok) <- parserConsume IDENTIFIER "Expect superclass method name." p2
          let superTok = parserPrevious p1
          Right (E_SUPER superTok methodTok, p3)

  | (True, p1) <- parserMatch [THIS] p0
      = Right (E_THIS (parserPrevious p1), p1)

  | (True, p1) <- parserMatch [IDENTIFIER] p0
      = Right (E_VARIABLE (parserPrevious p1), p1)

  | (True, p1) <- parserMatch [LEFT_PAREN] p0
      = do
          (expr, p2) <- parseExpression p1
          (p3, _)  <-
            parserConsume
              RIGHT_PAREN
              "Expect ')' after expression."
              p2
          Right (E_GROUPING expr, p3)

  | otherwise
      = let token = parserPeek p0
        in
          Left $
            loxError
              (t_line token)
              "Expected expression. Got " <> t_lexeme token

-------------------------------------------------------------------------------------------
--                                 Expression Evaluation 
-------------------------------------------------------------------------------------------

exprIsTruthy :: LoxValue -> Bool
exprIsTruthy lit
  | lit == L_NIL        = False
  | L_BOOL b <- lit     = b
  | otherwise           = True

exprIsEqual :: LoxValue -> LoxValue -> Bool
exprIsEqual a b
  | a == L_NIL && b == L_NIL = True
  | a == L_NIL = False
  | otherwise = a == b

exprUnary :: Token -> LoxValue -> EitherLit
exprUnary op lit =
  let line = t_line op
  in case t_type op of
    MINUS ->
      case lit of
        L_NUMBER n -> Right (L_NUMBER (-n))
        _ ->
           Left $
            loxError
            line
            "Unary minus on non-number"

    BANG ->
      Right (L_BOOL (not $ exprIsTruthy lit))

    _ ->
      Left $
        loxError
        line
        "Unknown unary operator " <> t_lexeme op

exprBinary :: LoxValue -> Token -> LoxValue -> EitherLit
exprBinary left op right =
  let line = t_line op
      tt   = t_type op
      lex  = t_lexeme op
  in case tt of
    EQUAL_EQUAL -> Right (L_BOOL (exprIsEqual left right))
    BANG_EQUAL  -> Right (L_BOOL (not (exprIsEqual left right)))
    _ ->
      case (left, right) of
        (L_NUMBER l, L_NUMBER r) ->
          case tt of
            MINUS         -> Right (L_NUMBER (l - r))
            PLUS          -> Right (L_NUMBER (l + r))
            SLASH         -> Right (L_NUMBER (l / r))
            STAR          -> Right (L_NUMBER (l * r))
            GREATER       -> Right (L_BOOL (l > r))
            GREATER_EQUAL -> Right (L_BOOL (l >= r))
            LESS          -> Right (L_BOOL (l < r))
            LESS_EQUAL    -> Right (L_BOOL (l <= r))
            _ ->
              Left $
                loxError
                line
                "Invalid numeric operator: " <> lex

        (L_STRING l, L_STRING r) ->
          case tt of
            PLUS -> Right (L_STRING (l <> r))
            _ ->
              Left $
                loxError
                line
                "Invalid string operator: " <> lex

        _ ->
          Left (loxError line
                ("Invalid Binary Operation ("
                 <> showLiteral left
                 <> " "
                 <> lex
                 <> " "
                 <> showLiteral right
                 <> ")"))

-- exprEval
-- add new one when adding to Expr
-- generic evaluator

exprEval :: Interpreter -> Expr -> EitherListBsLitInterp

exprEval i (E_LITERAL (Just lit)) =
  Right ([], lit, i)

exprEval i (E_LITERAL Nothing) =
  Right ([], L_NIL, i)

exprEval i (E_GROUPING e) =
  exprEval i e

exprEval env (E_UNARY op e) = do
  (outs1, v, env1) <- exprEval env e
  lit              <- exprUnary op v
  Right (outs1, lit, env1)

exprEval env (E_BINARY l op r) = do
  (outsL, lv, env1) <- exprEval env l
  (outsR, rv, env2) <- exprEval env1 r
  lit               <- exprBinary lv op rv
  let outs = outsL ++ outsR
  Right (outs, lit, env2)

exprEval interp (E_VARIABLE nameTok) = do
  let env = i_environment interp
  lit <- envGet env nameTok
  Right ([], lit, interp)


exprEval interp (E_ASSIGN name expr) = do
  (outs, val, interp1) <- exprEval interp expr
  env2 <- envAssign (i_environment interp1) name val
  let interp2 = interp1 { i_environment = env2 }
  Right (outs, val, interp2)

exprEval i (E_LOGICAL l op r) = do
  (outsL, lv, i1) <- exprEval i l
  case t_type op of
    OR ->
      if exprIsTruthy lv
        then Right (outsL, lv, i1)
        else do
          (outsR, rv, i2) <- exprEval i1 r
          let outs = outsL ++ outsR
          Right (outs, rv, i2)

    AND ->
      if not (exprIsTruthy lv)
        then Right (outsL, lv, i1)
        else do
          (outsR, rv, i2) <- exprEval i1 r
          let outs = outsL ++ outsR
          Right (outs, rv, i2)

exprEval i0 (E_CALL calleeExpr paren argExprs) = do
  (outsCallee, calleeLit, i1) <- exprEval i0 calleeExpr
  callable                    <- expectCallable calleeLit
  (i2, outsArgs, argLits)     <- collectArgs i1 argExprs [] []

  let arity   = lc_arity callable
      argSize = length argLits

  if arity /= argSize
    then
      Left
        ( loxError (t_line paren)
          ( "Expected "
          <> BS.pack (show arity)
          <> " arguments but got "
          <> BS.pack (show argSize)
          <> "."
          )
        )
    else do
      (outsFn, result, i3) <- lc_call callable i2 paren argLits
      let outs = outsArgs ++ outsCallee ++ outsFn
      Right (outs, result, i3)

  where
    collectArgs env [] outsAcc litAcc =
      Right (env, reverse outsAcc, reverse litAcc)

    collectArgs env (x:xs) outsAcc litAcc = do
      (outs1, lit, env1) <- exprEval env x
      let outsAcc' = outsAcc ++ outs1
      collectArgs env1 xs outsAcc' (lit : litAcc)

exprEval interp (E_GET objectExpr nameTok) = do
  (outs, objLit, interp1) <- exprEval interp objectExpr
  case objLit of
    L_INSTANCE inst -> do
      case instanceGet inst nameTok of
        Right lit -> Right (outs, lit, interp1)
        Left err  -> Left err
    _ ->
      Left (loxError (t_line nameTok) "Only instances have properties.")

exprEval interp (E_SET objectExpr nameTok valueExpr) = do
  (outs1, objLit, interp1) <- exprEval interp objectExpr
  (outs2, valLit, interp2) <- exprEval interp1 valueExpr

  case objLit of
    L_INSTANCE inst ->
      let inst' = instanceSet inst nameTok valLit
      in case objectExpr of
           E_VARIABLE varTok -> do
             let env0 = i_environment interp2
             env' <- envAssign env0 varTok (L_INSTANCE inst')
             let interp3 = interp2 { i_environment = env' }
             Right (outs1 ++ outs2, L_INSTANCE inst', interp3)

           E_THIS thisTok -> do
             let env0 = i_environment interp2
             env' <- envAssign env0 thisTok (L_INSTANCE inst')
             let interp3 = interp2 { i_environment = env' }
             Right (outs1 ++ outs2, L_INSTANCE inst', interp3)

           _ ->
             Right (outs1 ++ outs2, L_INSTANCE inst', interp2)

    _ ->
      Left (loxError (t_line nameTok) "Only instances have fields.")

exprEval interp (E_THIS tok) = do
  let env = i_environment interp
  case envGet env tok of
    Right lit -> Right ([], lit, interp)
    Left _    -> Left (loxError (t_line tok) "Can't use 'this' outside of a class.")

exprEval interp0 (E_SUPER superTok methodTok) = do
  let env = i_environment interp0

  superVal <- envGet env superTok
  klass <- case superVal of
    L_CLASS k -> pure k
    _         -> Left (loxError (t_line superTok) "'super' is not a class")

  let tokThis = Token
        { t_type    = IDENTIFIER
        , t_lexeme  = "this"
        , t_literal = Nothing
        , t_line    = t_line superTok
        }

  thisVal <- envGet env tokThis
  inst <- case thisVal of
    L_INSTANCE i -> pure i
    _            -> Left (loxError (t_line superTok) "'this' is not an instance")

  let methodName = t_lexeme methodTok
  case findMethod klass methodName of
    Nothing ->
      Left (loxError (t_line methodTok)
            ("Undefined property '" <> methodName <> "'"))
    Just fn -> do
      let bound = bindThis fn inst
      pure ([], L_FUNC bound, interp0)

---------------------------------------------------------------------------------------
--                                    Statements
---------------------------------------------------------------------------------------

data Stmt
  = S_BLOCK      [Stmt]                                       -- statements
  | S_CLASS      Token (Maybe Expr) [Stmt]                    -- name, superclass, methods
  | S_EXPRESSION Expr                                         -- expression
  | S_FUNCTION   Token [Token] [Stmt]                         -- name params body
  | S_IF         Expr Stmt (Maybe Stmt)                       -- condition thenBranch elseBranch
  | S_PRINT      Expr                                         -- expression
  | S_RETURN     Token (Maybe Expr)                           -- keyword value
  | S_VAR        Token (Maybe Expr)                           -- name initializer
  | S_WHILE      Expr Stmt                                    -- condition body
  | S_FOR        (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt  -- init cond inc body
  | S_BREAK
  | S_CONTINUE
  deriving (Show)

---------------------------------------------------------------------------------------
--                                      Classes
---------------------------------------------------------------------------------------

data LoxClass = LoxClass
  { lc_name    :: BS.ByteString
  , lc_super   :: Maybe LoxClass
  , lc_methods :: SM.Map BS.ByteString LoxFunction
  }

instance Show LoxClass where
  show klass = BS.unpack (lc_name klass)

data LoxInstance = LoxInstance
  { li_class  :: LoxClass
  , li_fields :: SM.Map BS.ByteString LoxValue
  }

instance Show LoxInstance where
  show inst = BS.unpack (lc_name (li_class inst)) ++ " instance"

classCallable klass =
  LoxCallable
    { lc_arity =
        case findMethod klass "init" of
          Just fn -> length (lf_params fn)
          Nothing -> 0

    , lc_call = \interp tok args ->
        let inst = LoxInstance klass SM.empty in
        case findMethod klass "init" of
          Just fn -> do
            let bound    = bindThis fn inst
                callable = wrapUserFunction bound
            (outs, retLit, interp1) <- lc_call callable interp tok args
            Right (outs, retLit, interp1)

          Nothing ->
            Right ([], L_INSTANCE inst, interp)
    }

bindThis :: LoxFunction -> LoxInstance -> LoxFunction
bindThis fn inst =
  let closure = lf_closure fn
      -- create a new environment whose parent is the closure
      env0    = newEnv closure
      env1    = envDefine env0 "this" (L_INSTANCE inst)
  in fn { lf_closure = env1 }

instanceGet :: LoxInstance -> Token -> Either BS.ByteString LoxValue
instanceGet inst nameTok =
  case SM.lookup (t_lexeme nameTok) (li_fields inst) of
    Just v  -> Right v
    Nothing ->
      case findMethod (li_class inst) (t_lexeme nameTok) of
        Just fn -> Right (L_FUNC (bindThis fn inst))
        Nothing ->
          Left (loxError (t_line nameTok)
                ("Undefined property '" <> t_lexeme nameTok <> "'"))

instanceSet :: LoxInstance -> Token -> LoxValue -> LoxInstance
instanceSet inst nameTok value =
  inst { li_fields = SM.insert (t_lexeme nameTok) value (li_fields inst) }

findMethod :: LoxClass -> BS.ByteString -> Maybe LoxFunction
findMethod klass name =
  case SM.lookup name (lc_methods klass) of
    Just fn -> Just fn
    Nothing ->
      case lc_super klass of
        Just super -> findMethod super name
        Nothing    -> Nothing

---------------------------------------------------------------------------------------
--                                 Statement Parsing
---------------------------------------------------------------------------------------

stmtPrint :: Parser -> EitherStmtParser
stmtPrint p0 = do
  (expr, p1) <- parseExpression p0
  (p2, _)    <-
    parserConsume
      SEMICOLON
      "Expect ';' after value."
      p1

  Right (S_PRINT expr, p2)

stmtExpression :: Parser -> EitherStmtParser
stmtExpression p0 = do
  (expr, p1) <- parseExpression p0
  (p2, _) <-
    parserConsume
      SEMICOLON
      "Expect ';' after expression."
      p1

  Right (S_EXPRESSION expr, p2)

stmtBlock :: Parser -> EitherListStmtParser
stmtBlock p0 = loop p0 []
  where
    loop p acc
      | not (parserCheck p RIGHT_BRACE)
        && not (parserIsAtEnd p) = do
          (stmt, p1) <- stmtDeclaration p
          loop p1 (stmt : acc)

      | otherwise = do
          (p2, _) <-
            parserConsume
              RIGHT_BRACE
              "Expect '}' after block."
              p

          Right (reverse acc, p2)

stmtIf :: Parser -> EitherStmtParser
stmtIf p0 = do
  (p1, _) <-
    parserConsume
      LEFT_PAREN
      "Expect '(' after 'if'."
      p0

  (cond, p2) <- parseExpression p1

  (p3, _) <-
    parserConsume
      RIGHT_PAREN
      "Expect ')' after if condition."
      p2

  (tb, p4) <- statement p3

  let (hasEq, p5) = parserMatch [ELSE] p4

  (eb, p6) <-
    if hasEq
      then do
        (expr, p') <- statement p5
        Right (Just expr, p')
      else
        Right (Nothing, p5)

  Right (S_IF cond tb eb, p6)

stmtWhile :: Parser -> EitherStmtParser
stmtWhile p0 = do
  (p1, _) <-
    parserConsume
      LEFT_PAREN
      "Expect '(' after 'while'."
      p0

  (cond, p2) <- parseExpression p1

  (p3, _) <-
    parserConsume
      RIGHT_PAREN
      "Expect ')' after condition."
      p2

  case parserMatch [LEFT_BRACE] p3 of
    (True, p4) -> do
      (stmts, p5) <- stmtBlock p4
      Right (S_WHILE cond (S_BLOCK stmts), p5)

    _ ->
      let line = t_line (parserPeek p3)
      in Left $
        loxError
          line
          "Expected '{' after while condition."

stmtFor :: Parser -> EitherStmtParser
stmtFor p0 = do
  (p1, _) <-
    parserConsume
      LEFT_PAREN
      "Expect '(' after 'for'."
      p0

  let (semi, pSemi) = parserMatch [SEMICOLON] p1
      (isVar, pVar) = parserMatch [VAR] p1

  (initStmt, p2) <-
    if semi then
      Right (Nothing, pSemi)
    else if isVar then do
      (s, p') <- stmtVarDeclaration pVar
      Right (Just s, p')
    else do
      (s, p') <- stmtExpression p1
      Right (Just s, p')

  (condExpr, p3) <-
    if not (parserCheck p2 SEMICOLON)
      then do
        (e, p3) <- parseExpression p2
        Right (Just e, p3)
      else
        Right (Nothing, p2)

  (p4, _) <-
    parserConsume
      SEMICOLON
      "Expect ';' after loop condition."
      p3

  (incExpr, p5) <-
    if not (parserCheck p4 RIGHT_PAREN)
      then do
        (e, p5) <- parseExpression p4
        Right (Just e, p5)
      else
        Right (Nothing, p4)

  (p6, _) <-
    parserConsume
      RIGHT_PAREN
      "Expect ')' after for clauses."
      p5

  (bodyStmt, p7) <- statement p6

  bodyBlock <-
    case bodyStmt of
      b@(S_BLOCK _) -> Right b
      _             -> Left "Expect '{' after for clauses."

  Right (S_FOR initStmt condExpr incExpr bodyBlock, p7)

stmtBreak :: Parser -> EitherStmtParser
stmtBreak p0 = do
  (p1, _) <-
    parserConsume
      SEMICOLON
      "Expect ';' after 'break'."
      p0

  Right (S_BREAK, p1)

stmtContinue :: Parser -> EitherStmtParser
stmtContinue p0 = do
  (p1, _) <-
    parserConsume
      SEMICOLON
      "Expect ';' after 'continue'."
      p0

  Right (S_CONTINUE, p1)

stmtReturn :: Parser -> EitherStmtParser
stmtReturn p0 = do
  let key = parserPrevious p0

  (val, p1) <-
    if not (parserCheck p0 SEMICOLON)
      then do
        (expr, p1) <- parseExpression p0
        Right (Just expr, p1)
      else
        Right (Nothing, p0)

  (p2, _) <-
    parserConsume
      SEMICOLON
      "Expect ';' after value"
      p1

  pure (S_RETURN key val, p2)


statement :: Parser -> EitherStmtParser
statement p0
  | (True, p1) <- parserMatch [FOR] p0      = stmtFor p1
  | (True, p1) <- parserMatch [IF] p0       = stmtIf p1
  | (True, p1) <- parserMatch [PRINT] p0    = stmtPrint p1
  | (True, p1) <- parserMatch [RETURN] p0   = stmtReturn p1
  | (True, p1) <- parserMatch [WHILE] p0    = stmtWhile p1
  | (True, p1) <- parserMatch [BREAK] p0    = stmtBreak p1
  | (True, p1) <- parserMatch [CONTINUE] p0 = stmtContinue p1

  | (True, p1) <- parserMatch [LEFT_BRACE] p0 = do
      (stmts, p2) <- stmtBlock p1
      Right (S_BLOCK stmts, p2)

  | otherwise =
      stmtExpression p0

stmtVarDeclaration :: Parser -> EitherStmtParser
stmtVarDeclaration p0 = do
  (p1, nameTok) <-
    parserConsume
      IDENTIFIER
      "Expect variable name."
      p0

  let (hasEq, p2) = parserMatch [EQUAL] p1

  (maybeInit, p3) <-
    if hasEq
      then do
        (expr, p') <- parseExpression p2
        Right (Just expr, p')
      else
        Right (Nothing, p2)

  (p4, _) <-
    parserConsume
      SEMICOLON
      "Expect ';' after variable declaration."
      p3

  Right (S_VAR nameTok maybeInit, p4)

stmtFunction :: Parser -> BS.ByteString -> EitherStmtParser
stmtFunction p0 kind = do
  (p1, name) <-
    parserConsume
      IDENTIFIER
      (  "Expect "
      <> kind
      <> " name."
      )
      p0

  (p2, _) <-
    parserConsume
      LEFT_PAREN
      (  "Expect '(' after "
      <> kind
      <> " name."
      )
      p1

  (p3, params) <-
    if parserCheck p2 RIGHT_PAREN
      then Right (p2, [])
      else do
        (pFirst, firstParam) <-
          parserConsume IDENTIFIER "Expect parameter name." p2

        (pRest, moreParams) <- loop pFirst 1 [firstParam]

        Right (pRest, moreParams)

  (p4, _) <-
    parserConsume
      RIGHT_PAREN
      "Expect ')' after parameters."
      p3

  (p5, _) <-
    parserConsume
      LEFT_BRACE
      (  "Expect '{' before "
      <> kind
      <> " body."
      )
      p4

  (body, p6) <- stmtBlock p5
  Right (S_FUNCTION name params body, p6)

  where
     loop p0 len acc
      | (True, p1) <- parserMatch [COMMA] p0
          = if len >= 255
              then
                Left $
                 loxError
                  (t_line $ parserPeek p1)
                  "Can't have more than 255 parameters."
              else do
                (p2, name) <-
                  parserConsume
                    IDENTIFIER
                    "Expect parameter name"
                    p1

                loop p2 (len + 1) (name : acc)

      | otherwise = Right (p0, reverse acc)

stmtClassDeclaration :: Parser -> EitherStmtParser
stmtClassDeclaration p0 = do
  (p1, nameTok) <-
    parserConsume IDENTIFIER "Expect class name." p0

  let (ok, p2) = parserMatch [LESS] p1
  (p3, superclass) <-
      if ok
        then do
          (pSuper, superNameTok) <-
            parserConsume IDENTIFIER "Expect superclass name." p2
          Right (pSuper, Just (E_VARIABLE superNameTok))
        else
          Right (p2, Nothing)

  case superclass of
    Just (E_VARIABLE superTok)
      | t_lexeme superTok == t_lexeme nameTok ->
          Left $
            loxError
              (t_line superTok)
              "A class can't inherit from itself."
    _ -> pure ()

  let pBrace = if ok then p3 else p2
  (p4, _) <-
    parserConsume LEFT_BRACE "Expect '{' before class body." pBrace

  let loop p acc =
        if parserCheck p RIGHT_BRACE || parserIsAtEnd p
          then Right (reverse acc, p)
          else do
            (method, p1) <- stmtFunction p "method"
            loop p1 (method : acc)

  (methods, p5) <- loop p4 []

  (p6, _) <-
    parserConsume RIGHT_BRACE "Expect '}' after class body." p5

  Right (S_CLASS nameTok superclass methods, p6)

stmtDeclaration :: Parser -> EitherStmtParser
stmtDeclaration p0 =
  case parserMatch [CLASS] p0 of
    (True, p1) -> stmtClassDeclaration p1
    _ ->
      case parserMatch [FUNC] p0 of
        (True, p1) -> stmtFunction p1 "function"
        _ ->
          case parserMatch [VAR] p0 of
            (True, p1) -> stmtVarDeclaration p1
            _          -> statement p0

stmtParse :: Parser -> EitherListStmtParser
stmtParse p0 = go p0 []
  where
    go p acc
      | parserIsAtEnd p = Right (reverse acc, p)
      | otherwise = do
          (stmt, p1) <- stmtDeclaration p
          go p1 (stmt : acc)

---------------------------------------------------------------------------------------
--                               Statement Evaluation
---------------------------------------------------------------------------------------

data ExecResult
  = ER_NORMAL   Interpreter
  | ER_BREAK    Interpreter
  | ER_CONTINUE Interpreter
  | ER_RETURN   LoxValue Interpreter

execBlock :: Interpreter -> [BS.ByteString] -> [Stmt] -> EitherListBsExecRes
execBlock interp0 = go interp1
  where
    env0    = i_environment interp0
    envNew  = newEnv env0
    interp1 = interp0 { i_environment = envNew }

    go interp acc [] =
      let env = i_environment interp in
      case e_enclosing env of
        Just parent ->
          let parent'  = parent { e_loop_depth = e_loop_depth env }
              interp'  = interp { i_environment = parent' }
          in Right (acc, ER_NORMAL interp')

        Nothing ->
          let env0'   = env0 { e_loop_depth = e_loop_depth env }
              interp' = interp { i_environment = env0' }
          in Right (acc, ER_NORMAL interp')

    go interp acc (s:ss) = do
      (outs1, res1) <- stmtExec interp s
      let acc' = acc ++ outs1
      case res1 of
        ER_NORMAL interp1   -> go interp1 acc' ss
        ER_BREAK  interp1   -> Right (acc', ER_BREAK interp1)
        ER_CONTINUE interp1 -> Right (acc', ER_CONTINUE interp1)
        ER_RETURN lit interp1 ->
          Right (acc', ER_RETURN lit interp1)

-- stmtExec
-- add new one when adding to Stmt
-- generic executor for Stmt

stmtExec
  :: Interpreter
  -> Stmt
  -> EitherListBsExecRes

stmtExec i0 (S_PRINT expr) = do
  (outsE, lit, i1) <- exprEval i0 expr
  let outs = outsE ++ [showLiteral lit]
  Right (outs, ER_NORMAL i1)

stmtExec interp0 (S_EXPRESSION expr) = do
  (outs, _, interp1) <- exprEval interp0 expr
  Right (outs, ER_NORMAL interp1)

stmtExec interp0 (S_VAR name maybeInit) =
  case maybeInit of
    Nothing ->
      let env0    = i_environment interp0
          env1    = envDefine env0 (t_lexeme name) L_NIL
          interp1 = interp0 { i_environment = env1 }
      in Right ([], ER_NORMAL interp1)

    Just expr -> do
      (outs, lit, interp1) <- exprEval interp0 expr
      let env1    = i_environment interp1
          env2    = envDefine env1 (t_lexeme name) lit
          interp2 = interp1 { i_environment = env2 }
      Right (outs, ER_NORMAL interp2)

stmtExec interp0 (S_BLOCK stmts) =
  execBlock interp0 [] stmts

stmtExec interp0 (S_IF cond tb eb) = do
  (outsCond, lit, interp1) <- exprEval interp0 cond
  case exprIsTruthy lit of
    True -> do
      (outsThen, resThen) <- stmtExec interp1 tb
      Right (outsCond ++ outsThen, resThen)
    False ->
      case eb of
        Just v -> do
          (outsElse, resElse) <- stmtExec interp1 v
          Right (outsCond ++ outsElse, resElse)
        Nothing ->
          Right (outsCond, ER_NORMAL interp1)

stmtExec interp0 (S_WHILE cond body) =
  loop interpStart []
  where
    interpStart =
      let env0 = i_environment interp0
          env1 = enterLoop env0
      in interp0 { i_environment = env1 }

    loop interp accOuts = do
      (outsCond, lit, interp1) <- exprEval interp cond

      if not (exprIsTruthy lit)
        then
          let env1    = i_environment interp1
              env2    = exitLoop env1
              interp2 = interp1 { i_environment = env2 }
          in Right (accOuts ++ outsCond, ER_NORMAL interp2)

        else do
          (outsBody, resBody) <- stmtExec interp1 body
          let acc1 = accOuts ++ outsCond ++ outsBody

          case resBody of
            ER_NORMAL interp2 -> do
              (outsLoop, resLoop) <- loop interp2 []
              Right (acc1 ++ outsLoop, resLoop)

            ER_CONTINUE interp2 -> do
              (outsLoop, resLoop) <- loop interp2 []
              Right (acc1 ++ outsLoop, resLoop)

            ER_BREAK interp2 ->
              let env2    = i_environment interp2
                  env3    = exitLoop env2
                  interp3 = interp2 { i_environment = env3 }
              in Right (acc1, ER_NORMAL interp3)

            ER_RETURN litR interp2 ->
              Right (acc1, ER_RETURN litR interp2)

stmtExec interp0 (S_FOR init cond inc body) = do
  (outsInit, resInit) <-
    case init of
      Nothing   -> Right ([], ER_NORMAL interp0)
      Just stmt -> stmtExec interp0 stmt

  case resInit of
    ER_NORMAL interp1 ->
      let env1 = i_environment interp1
          env2 = enterLoop env1
          interpStart = interp1 { i_environment = env2 }
      in loop interpStart outsInit

    ER_BREAK interp1    -> Right (outsInit, ER_NORMAL interp1)
    ER_CONTINUE interp1 -> Right (outsInit, ER_NORMAL interp1)
    ER_RETURN l interp1 -> Right (outsInit, ER_RETURN l interp1)

  where
    loop interp accOuts = do
      (outsCond, condVal, interpCond) <-
        case cond of
          Nothing   -> Right ([], L_BOOL True, interp)
          Just expr -> exprEval interp expr

      if not (exprIsTruthy condVal)
        then
          let envC    = i_environment interpCond
              envE    = exitLoop envC
              interpE = interpCond { i_environment = envE }
          in Right (accOuts ++ outsCond, ER_NORMAL interpE)

        else do
          (outsBody, resBody) <- stmtExec interpCond body
          let acc1 = accOuts ++ outsCond ++ outsBody

          case resBody of
            ER_NORMAL interpB -> do
              (outsInc, interpAfterInc) <-
                case inc of
                  Nothing   -> Right ([], interpB)
                  Just expr -> do
                    (outsI, _, interpI) <- exprEval interpB expr
                    Right (outsI, interpI)

              loop interpAfterInc (acc1 ++ outsInc)

            ER_CONTINUE interpB -> do
              (outsInc, interpAfterInc) <-
                case inc of
                  Nothing   -> Right ([], interpB)
                  Just expr -> do
                    (outsI, _, interpI) <- exprEval interpB expr
                    Right (outsI, interpI)

              loop interpAfterInc (acc1 ++ outsInc)

            ER_BREAK interpB ->
              let envB    = i_environment interpB
                  envE    = exitLoop envB
                  interpE = interpB { i_environment = envE }
              in Right (acc1, ER_NORMAL interpE)

            ER_RETURN litR interpB ->
              Right (acc1, ER_RETURN litR interpB)

stmtExec interp S_BREAK =
  let env = i_environment interp in
  if e_loop_depth env == 0
    then Left (loxError 0 "break outside loop")
    else Right ([], ER_BREAK interp)

stmtExec interp S_CONTINUE =
  let env = i_environment interp in
  if e_loop_depth env == 0
    then Left (loxError 0 "continue outside loop")
    else Right ([], ER_CONTINUE interp)

stmtExec interp0 (S_FUNCTION name params body) =
  let env0 = i_environment interp0
      env1 = envDefine env0 (t_lexeme name) (L_FUNC fn)
      fn   = newLoxFunction name params body env1 False
      interp1 = interp0 { i_environment = env1 }
  in Right ([], ER_NORMAL interp1)

stmtExec interp0 (S_CLASS name super methods) = do
  let env0 = i_environment interp0
      env1 = envDefine env0 (t_lexeme name) L_NIL
      interp1 = interp0 { i_environment = env1 }

  mSuper <- case super of
    Nothing ->
      pure Nothing

    Just (E_VARIABLE superTok) -> do
      (outs, val, interp2) <- exprEval interp1 (E_VARIABLE superTok)
      case val of
        L_CLASS klass -> do
          let LoxClass superName _ _ = klass
          if superName == t_lexeme name
            then Left (loxError (t_line name) "A class can't inherit from itself.")
            else pure (Just klass)

        _ ->
          Left (loxError (t_line superTok) "Superclass must be a class.")

    Just _ ->
      Left (loxError (t_line name) "Superclass expression must be a variable.")

  (envForMethods, interp2) <-
    case mSuper of
      Nothing ->
        pure (env1, interp1)

      Just superClass -> do
        let envSuper = newEnv env1
            envSuper' = envDefine envSuper "super" (L_CLASS superClass)
            interpSuper = interp1 { i_environment = envSuper' }
        pure (envSuper', interpSuper)

  let methodMap =
        foldl'
          (\m stmt ->
             case stmt of
               S_FUNCTION fname params body ->
                 let isInit = t_lexeme fname == "init"
                     fn     = newLoxFunction fname params body envForMethods isInit
                 in SM.insert (t_lexeme fname) fn m
               _ -> m
          )
          SM.empty
          methods

  let klass = LoxClass
        { lc_name    = t_lexeme name
        , lc_super   = mSuper
        , lc_methods = methodMap
        }

      interpFinal =
        case mSuper of
          Nothing -> interp2
          Just _  -> interp2 { i_environment = env1 }

  case envAssign (i_environment interpFinal) name (L_CLASS klass) of
    Left err ->
      Left err

    Right envFinal ->
      let interpFinal' = interpFinal { i_environment = envFinal }
      in pure ([], ER_NORMAL interpFinal')

stmtExec interp (S_RETURN _ Nothing) =
  Right ([], ER_RETURN L_NIL interp)

stmtExec interp0 (S_RETURN _ (Just expr)) = do
  (bs, val, interp1) <- exprEval interp0 expr
  Right (bs, ER_RETURN val interp1)

--------------------------------------------------------------------------------------
--                                    Builtin 
--------------------------------------------------------------------------------------

builtinClock :: LoxCallable
builtinClock =
  LoxCallable
    { lc_arity = 0
    , lc_call  = \interp _ _ ->
        unsafePerformIO $ do
          t <- getPOSIXTime
          pure (Right ([], L_NUMBER (realToFrac t), interp))
    }

builtinEcho :: LoxCallable
builtinEcho =
  LoxCallable
    { lc_arity = 1
    , lc_call  = \interp _ args ->
        case args of
          [lit] ->
            let out = showLiteral lit
            in Right ([out], L_NIL, interp)

          _ ->
            Left "print() expects exactly 1 argument."
    }

builtinMin :: LoxCallable
builtinMin =
  LoxCallable
    { lc_arity = 2
    , lc_call  = \interp tok args ->
        case args of
          [L_NUMBER a, L_NUMBER b] ->
            Right ([], L_NUMBER (min a b), interp)

          _ ->
            Left $
              loxError
                (t_line tok)
                "min() expects exactly 2 number arguments."
    }

builtinMax :: LoxCallable
builtinMax =
  LoxCallable
    { lc_arity = 2
    , lc_call  = \interp tok args ->
        case args of
          [L_NUMBER a, L_NUMBER b] ->
            Right ([], L_NUMBER (max a b), interp)

          _ ->
            Left $
              loxError
                (t_line tok)
                "max() expects exactly 2 number arguments."
    }

builtinMod :: LoxCallable
builtinMod =
  LoxCallable
    { lc_arity = 2
    , lc_call  = \interp tok args ->
        case args of
          [L_NUMBER a, L_NUMBER b] ->
            let ai  = round a :: Int
                bi  = round b :: Int
                out = ai `mod` bi
            in Right ([], L_NUMBER (fromIntegral out), interp)

          _ ->
            Left $
              loxError
                (t_line tok)
                "mod() expects exactly 2 number arguments."
    }

builtinToStr :: LoxCallable
builtinToStr =
  LoxCallable
    { lc_arity = 1
    , lc_call  = \interp tok args ->
        case args of
          [a] ->
            Right ([], L_STRING (showLiteral a), interp)

          _ ->
            Left $
              loxError
                (t_line tok)
                "string() expects exactly 1 literal argument."
    }

builtinToNum :: LoxCallable
builtinToNum =
  LoxCallable
    { lc_arity = 1
    , lc_call  = \interp tok args ->
        case args of
          [L_STRING a] ->
              Right ([], L_NUMBER (fromMaybe 0 (bsToDouble a)), interp)

          _ ->
            Left $
              loxError
                (t_line tok)
                "double() expects exactly 1 string argument."
    }

builtIns :: [(BS.ByteString, LoxCallable)]
builtIns =
  [ ("clock",  builtinClock)
  , ("echo",   builtinEcho)
  , ("min",    builtinMin)
  , ("max",    builtinMax)
  , ("mod",    builtinMod)
  , ("string", builtinToStr)
  , ("double", builtinToNum)
  ]

---------------------------------------------------------------------------------------
--                                    Run
---------------------------------------------------------------------------------------

interpRun
  :: Interpreter
  -> Parser
  -> ([BS.ByteString], Parser, Interpreter)
interpRun env p0 = go env p0 []
  where
    go env p acc
      | parserIsAtEnd p = (acc, p, env)
      | otherwise =
          case stmtDeclaration p of
            Left err ->
              let pSync = synchronize p
              in (acc ++ [err], pSync, env)

            Right (stmt, p1) ->
              case stmtExec env stmt of
                Left err ->
                  (acc ++ [err], p1, env)

                Right (outs, res) ->
                  let env' =
                        case res of
                          ER_NORMAL e    -> e
                          ER_BREAK e     -> e
                          ER_CONTINUE e  -> e
                          ER_RETURN _ e  -> e
                  in go env' p1 (acc ++ outs)

run :: Interpreter -> String -> IO (Interpreter, [BS.ByteString])
run interp source = do
  let sc0 = initScanner source 1
      sc1 = scannerRun sc0

  case s_errors sc1 of
    errs@(_:_) ->
      pure (interp, errs)

    [] -> do
      let parser0 = createParser (s_tokens sc1)
          (outs, _, interp1) = interpRun interp parser0
      pure (interp1, outs)

runFile :: FilePath -> IO ()
runFile path = do
  bytes <- readFile path
  (_, outs) <- run initInterpreter bytes
  mapM_ BS.putStrLn outs

runPrompt :: IO ()
runPrompt = do
  putStrLn "-----------------------------------------"
  putStrLn "                 LOX REPL                "
  putStrLn "-----------------------------------------"
  loop initInterpreter
  where
    loop interp = do
      putStr ">>> "
      hFlush stdout
      line <- getLine

      if line == "exit"
        then pure ()
        else do
          (interp', outs) <- run interp line
          mapM_ BS.putStrLn outs
          loop interp'

---------------------------------------------------------------------------------------
--                                      MAIN
---------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> runPrompt
    [p] -> runFile p
    _ -> putStrLn "Too many args. More than 1 arg not permitted."
