{-# LANGUAGE OverloadedStrings #-}
module Scanner where

import Types
import Helpers
import Data.Char  (isDigit, isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS

tokenCreate :: TokenType -> BS.ByteString -> Maybe LoxValue -> Int -> Token
tokenCreate t_type t_lexeme t_loxValue t_line =
  Token { t_type, t_lexeme, t_loxValue, t_line }

locInit :: Int -> Loc
locInit l = Loc { l_start = 0, l_curr = 0, l_line = l }

scannerInit :: String -> Int -> Scanner
scannerInit s_source line =
  Scanner
    { s_source = BS.pack s_source
    , s_tokens = []
    , s_loc    = locInit line
    , s_errors = []
    }

loxValueShow :: LoxValue -> BS.ByteString
loxValueShow (L_NUMBER n)      = formatNum n 10
loxValueShow (L_STRING s)      = s
loxValueShow (L_BOOL b)        = if b then "true" else "false"
loxValueShow L_NIL             = "nil"
loxValueShow (L_FUN _)         = "<fn>"
loxValueShow (L_CALL _)        = "<native fn>"
loxValueShow (L_CLASS klass)   = lc_name klass
loxValueShow (L_INSTANCE inst) = BS.pack (show inst)

data LocField = LocCurr | LocLine
locUpdate :: (Int -> Int) -> Scanner -> LocField -> Scanner
locUpdate f sc LocCurr =
  let loc = s_loc sc
  in sc { s_loc = loc { l_curr = f (l_curr loc) } }

locUpdate f sc LocLine =
  let loc = s_loc sc
  in sc { s_loc = loc { l_line = f (l_line loc) } }

setLocStart :: Scanner -> Scanner
setLocStart sc =
  let loc = s_loc sc
  in sc { s_loc = loc { l_start = l_curr loc } }

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
charAtScannerCur sc = BS.index (s_source sc) (l_curr $ s_loc sc)

charAtScannerCurOff :: Scanner -> Int -> Char
charAtScannerCurOff sc off = BS.index (s_source sc) (l_curr (s_loc sc) + off)

scannerIsAtEnd :: Scanner -> Bool
scannerIsAtEnd sc = l_curr (s_loc sc) >= scannerSourceLen sc

scannerAdvance :: Scanner -> (Scanner, Char)
scannerAdvance sc0 =
  let c   = charAtScannerCur sc0
      sc1 = locUpdate (+1) sc0 LocCurr
  in (sc1, c)

scannerPeek :: Scanner -> Char
scannerPeek sc
  | scannerIsAtEnd sc = '\0'
  | otherwise = charAtScannerCur sc

scannerPeekNext :: Scanner -> Char
scannerPeekNext sc
  | n_loc >= BS.length (s_source sc) = '\0'
  | otherwise = BS.index (s_source sc) n_loc

  where n_loc = l_curr (s_loc sc) + 1

scannerMatch :: Scanner -> Char -> (Scanner, Bool)
scannerMatch sc expected
  | scannerIsAtEnd sc = (sc, False)
  | charAtScannerCur sc /= expected = (sc, False)
  | otherwise = (locUpdate (+1) sc LocCurr, True)

scannerAddTokenWithLoxVal :: TokenType -> Maybe LoxValue -> Scanner -> Scanner
scannerAddTokenWithLoxVal t_type loxVal sc =
  let text  = sliceScannerStartCurrent sc
      token = tokenCreate t_type text loxVal (l_line $ s_loc sc)
  in sc { s_tokens = token : s_tokens sc }

scannerAddToken :: TokenType -> Scanner -> Scanner
scannerAddToken t_type = scannerAddTokenWithLoxVal t_type Nothing

scanForSlashes :: Scanner -> Scanner
scanForSlashes sc0 =
  let (sc1, matched) = scannerMatch sc0 '/'
  in if matched
       then consumeComment sc1
       else scannerAddTokenWithLoxVal SLASH Nothing sc0
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
              then locUpdate (+1) sc0 LocLine
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
      in scannerAddTokenWithLoxVal STRING lit sc1

scanForNumbers :: Scanner -> Scanner
scanForNumbers sc0 =
  let sc1 = consumeDigits sc0
      sc2 =
        if scannerPeek sc1 == '.' && isDigit (scannerPeekNext sc1)
          then fst (scannerAdvance sc1)
          else sc1

      sc3 = consumeDigits sc2
  in
    scannerAddTokenWithLoxVal NUMBER (parseSourceToDouble sc3) sc3

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
    keyword "fun"      = Just FUN
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
        '\n' -> locUpdate (+1) sc1 LocLine

        -- 1-2 chars
        '+'  ->
          let (scNext, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then PLUS_EQUAL else PLUS) scNext
        '-'  ->
          let (scNext, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then MINUS_EQUAL else MINUS) scNext

        '!' ->
          let (scNext, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then BANG_EQUAL else BANG) scNext
        '=' ->
          let (scNext, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then EQUAL_EQUAL else EQUAL) scNext
        '<' ->
          let (scNext, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then LESS_EQUAL else LESS) scNext
        '>' ->
          let (scNext, matched) = scannerMatch sc1 '='
          in scannerAddToken (if matched then GREATER_EQUAL else GREATER) scNext

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
      let eofTok   = tokenCreate EOF BS.empty Nothing (l_line $ s_loc sc0)
          sc1 = sc0 { s_tokens = eofTok : s_tokens sc0 }
      in check $ reverseTokensErrorsFromScanner sc1

  | otherwise =
      let sc1 = setLocStart sc0
          sc2 = scanToken sc1
      in scanTokens sc2

  where
    initToken = tokenCreate EOF "" Nothing 1
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
  putStrLn $ "  value   = " ++ show (t_loxValue token)
  putStrLn $ "  line    = " ++ show (t_line token)

data PpScanOp = PpErrors | PpTokens | PpTokensSource | PpTokensErrors | PpAll
prettyPrintScanner :: Scanner -> PpScanOp -> IO ()
prettyPrintScanner sc PpTokens = mapM_ prettyPrintToken (s_tokens sc)
prettyPrintScanner sc PpErrors = mapM_ BS.putStrLn (s_errors sc)

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
