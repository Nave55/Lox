{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Types
import Helpers

import qualified Data.ByteString.Char8 as BS

createParser :: [Token] -> Parser
createParser [] = error "createParser requires a non empty list"
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
parserAdvance (Parser [] _ _) = error "parserAdvance requires a non empty list"
parserAdvance (Parser (t:ts) _ cur) = Parser ts cur t

parserCheck :: Parser -> TokenType -> Bool
parserCheck parser tt = not (parserIsAtEnd parser) && (t_type (parserPeek parser) == tt)

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

    syncStarters = [ CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN ]

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

--------------------------------------------------------------------------------------
--                               Expression parsing 
--------------------------------------------------------------------------------------

parseExpression :: Parser -> EitherExprParser
parseExpression = parseAssignment

parseAssignment :: Parser -> EitherExprParser
parseAssignment p0 = do
  (expr, p1) <- parseOr p0
  let (matched, p2) = parserMatch [EQUAL, PLUS_EQUAL, MINUS_EQUAL] p1

  if matched then do
    let opTok    = parserPrevious p2
        op       = t_type opTok
        plusTok  = opTok { t_type = PLUS  }
        minusTok = opTok { t_type = MINUS }

    (value, p3) <- parseAssignment p2

    case expr of
      E_VARIABLE tok ->
        case op of
          EQUAL -> Right (E_ASSIGN tok value, p3)
          PLUS_EQUAL -> Right (E_ASSIGN tok (E_BINARY expr plusTok value), p3)
          MINUS_EQUAL -> Right (E_ASSIGN tok (E_BINARY expr minusTok value), p3)
          _ -> Left (loxError (t_line opTok) "Unknown assignment operator")

      E_GET obj nameTok -> Right (E_SET obj nameTok value, p3)
      _ -> Left (loxError (t_line opTok) "Invalid assignment target")

  else
    Right (expr, p1)

parseEquality :: Parser -> EitherExprParser
parseEquality p0 = do
  (expr, p1) <- parseComparison p0
  parserMatchRest
    expr p1 [BANG_EQUAL, EQUAL_EQUAL] parseComparison Binary

parseComparison :: Parser -> EitherExprParser
parseComparison p0 = do
  (expr, p1) <- parseTerm p0
  parserMatchRest
    expr p1 [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] parseTerm Binary

parseTerm :: Parser -> EitherExprParser
parseTerm p0 = do
  (expr, p1) <- parseFactor p0
  parserMatchRest expr p1 [MINUS, PLUS] parseFactor Binary

parseFactor :: Parser -> EitherExprParser
parseFactor p0 = do
  (expr, p1) <- parseUnary p0
  parserMatchRest expr p1 [SLASH, STAR] parseUnary Binary

parseAnd :: Parser -> EitherExprParser
parseAnd p0 = do
  (expr, p1) <- parseEquality p0
  parserMatchRest expr p1 [AND] parseEquality Logical

parseOr :: Parser -> EitherExprParser
parseOr p0 = do
  (expr, p1) <- parseAnd p0
  parserMatchRest expr p1 [OR] parseAnd Logical

parseCall :: Parser -> EitherExprParser
parseCall p0 = do
  (expr0, p1) <- parsePrimary p0
  loop expr0 p1
  where
    loop expr pIn = do
      case parserMatch [LEFT_PAREN] pIn of
        (True, pAfterParen) -> do
          (exprCall, pAfterCall) <- finishCall expr pAfterParen
          loop exprCall pAfterCall

        (False, _) ->
          case parserMatch [DOT] pIn of
            (True, pAfterDot) -> do
              (pAfterName, nameTok) <-
                parserConsume IDENTIFIER "Expect property name after '.'." pAfterDot
              loop (E_GET expr nameTok) pAfterName

            (False, _) -> Right (expr, pIn)

    finishCall callee pIn = do
      (pArgs, args) <- fcLoop pIn 0 []
      (pClose, parenTok) <-
        parserConsume RIGHT_PAREN "Expect ')' after arguments." pArgs
      Right (E_CALL callee parenTok (reverse args), pClose)

    fcLoop :: Parser -> Int -> [Expr] -> Either BS.ByteString (Parser, [Expr])
    fcLoop pIn len acc
      | not (parserCheck pIn RIGHT_PAREN) =
          if len >= 255
            then Left $
              loxError (t_line $ parserPeek pIn) "Can't have more than 255 arguments."
            else do
              (expr, pExpr) <- parseExpression pIn
              let (matched, pComma) = parserMatch [COMMA] pExpr
              if matched
                then fcLoop pComma (len + 1) (expr : acc)
                else Right (pExpr, expr : acc)

      | otherwise = Right (pIn, reverse acc)

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
  | (True, p1) <- parserMatch [FALSE] p0 = Right (E_LITERAL (Just (L_BOOL False)), p1)
  | (True, p1) <- parserMatch [TRUE] p0 = Right (E_LITERAL (Just (L_BOOL True)), p1)
  | (True, p1) <- parserMatch [NIL] p0 = Right (E_LITERAL Nothing, p1)

  | (True, p1) <- parserMatch [NUMBER, STRING] p0
      = case t_loxValue (parserPrevious p1) of
        Just lit -> Right (E_LITERAL (Just lit), p1)
        Nothing  -> Left "Expected literal"

  | (True, p1) <- parserMatch [SUPER] p0
      = do
          (p2, _) <- parserConsume DOT "Expect '.' after 'super'." p1
          (p3, methodTok) <- parserConsume IDENTIFIER "Expect superclass method name." p2
          let superTok = parserPrevious p1
          Right (E_SUPER superTok methodTok, p3)

  | (True, p1) <- parserMatch [THIS] p0 = Right (E_THIS (parserPrevious p1), p1)
  | (True, p1) <- parserMatch [IDENTIFIER] p0 = Right (E_VARIABLE (parserPrevious p1), p1)
  | (True, p1) <- parserMatch [LEFT_PAREN] p0
      = do
          (expr, p2) <- parseExpression p1
          (p3, _)  <- parserConsume RIGHT_PAREN "Expect ')' after expression." p2
          Right (E_GROUPING expr, p3)

  | otherwise
      = let token = parserPeek p0
        in
          Left $ loxError (t_line token) "Expected expression. Got " <> t_lexeme token

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

exprUnary :: Token -> LoxValue -> EitherLoxValue
exprUnary op lit =
  let line = t_line op
  in case t_type op of
    MINUS ->
      case lit of
        L_NUMBER n -> Right (L_NUMBER (-n))
        _ -> Left $ loxError line "Unary minus on non-number"

    BANG -> Right (L_BOOL (not $ exprIsTruthy lit))
    _    -> Left $ loxError line "Unknown unary operator " <> t_lexeme op

exprBinary :: LoxValue -> Token -> LoxValue -> EitherLoxValue
exprBinary left op right =
  let line   = t_line op
      tt     = t_type op
      lexeme = t_lexeme op
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
            _ -> Left $ loxError line "Invalid numeric operator: " <> lexeme

        (L_STRING l, L_STRING r) ->
          case tt of
            PLUS -> Right (L_STRING (l <> r))
            _ -> Left $ loxError line "Invalid string operator: " <> lexeme

        _ ->
          Left (loxError line
                ("Invalid Binary Operation ("
                 <> loxValueShow left
                 <> " "
                 <> lexeme
                 <> " "
                 <> loxValueShow right
                 <> ")"))


---------------------------------------------------------------------------------------
--                                 Statement Parsing
---------------------------------------------------------------------------------------

stmtPrint :: Parser -> EitherStmtParser
stmtPrint p0 = do
  (expr, p1) <- parseExpression p0
  (p2, _)    <- parserConsume SEMICOLON "Expect ';' after value." p1
  Right (S_PRINT expr, p2)

stmtExpression :: Parser -> EitherStmtParser
stmtExpression p0 = do
  (expr, p1) <- parseExpression p0
  (p2, _)    <- parserConsume SEMICOLON "Expect ';' after expression." p1
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
          (p2, _) <- parserConsume RIGHT_BRACE "Expect '}' after block." p
          Right (reverse acc, p2)

stmtIf :: Parser -> EitherStmtParser
stmtIf p0 = do
  (p1, _)    <- parserConsume LEFT_PAREN "Expect '(' after 'if'." p0
  (cond, p2) <- parseExpression p1
  (p3, _)    <- parserConsume RIGHT_PAREN "Expect ')' after if condition." p2
  (tb, p4)   <- statement p3

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
  (p1, _)    <- parserConsume LEFT_PAREN "Expect '(' after 'while'." p0
  (cond, p2) <- parseExpression p1
  (p3, _)    <- parserConsume RIGHT_PAREN "Expect ')' after condition." p2

  case parserMatch [LEFT_BRACE] p3 of
    (True, p4) -> do
      (stmts, p5) <- stmtBlock p4
      Right (S_WHILE cond (S_BLOCK stmts), p5)

    _ ->
      let line = t_line (parserPeek p3)
      in Left $ loxError line "Expected '{' after while condition."

stmtFor :: Parser -> EitherStmtParser
stmtFor p0 = do
  (p1, _) <- parserConsume LEFT_PAREN "Expect '(' after 'for'." p0

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

  (p4, _) <- parserConsume SEMICOLON "Expect ';' after loop condition." p3

  (incExpr, p5) <-
    if not (parserCheck p4 RIGHT_PAREN)
      then do
        (e, p5) <- parseExpression p4
        Right (Just e, p5)
      else
        Right (Nothing, p4)

  (p6, _)        <- parserConsume RIGHT_PAREN "Expect ')' after for clauses." p5
  (bodyStmt, p7) <- statement p6

  bodyBlock <-
    case bodyStmt of
      b@(S_BLOCK _) -> Right b
      _             -> Left "Expect '{' after for clauses."

  Right (S_FOR initStmt condExpr incExpr bodyBlock, p7)

stmtBreak :: Parser -> EitherStmtParser
stmtBreak p0 = do
  (p1, _) <- parserConsume SEMICOLON "Expect ';' after 'break'." p0
  Right (S_BREAK, p1)

stmtContinue :: Parser -> EitherStmtParser
stmtContinue p0 = do
  (p1, _) <- parserConsume SEMICOLON "Expect ';' after 'continue'." p0
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

  (p2, _) <- parserConsume SEMICOLON "Expect ';' after value" p1

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

  | otherwise = stmtExpression p0

stmtVarDeclaration :: Parser -> EitherStmtParser
stmtVarDeclaration p0 = do
  (p1, nameTok)  <- parserConsume IDENTIFIER "Expect variable name." p0
  let (hasEq, p2) = parserMatch [EQUAL] p1

  (maybeInit, p3) <-
    if hasEq
      then do
        (expr, p') <- parseExpression p2
        Right (Just expr, p')
      else
        Right (Nothing, p2)

  (p4, _) <- parserConsume SEMICOLON "Expect ';' after variable declaration." p3

  Right (S_VAR nameTok maybeInit, p4)
stmtFunction :: Parser -> BS.ByteString -> EitherStmtParser
stmtFunction p0 kind = do
  (p1, name) <- parserConsume IDENTIFIER ("Expect " <> kind <> " name.") p0
  (p2, _)    <- parserConsume LEFT_PAREN ("Expect '(' after " <> kind <> " name.") p1

  (p3, params) <-
    if parserCheck p2 RIGHT_PAREN
      then Right (p2, [])
      else do
        (pFirst, firstParam) <- parserConsume IDENTIFIER "Expect parameter name." p2
        (pRest, moreParams)  <- loop pFirst (1 :: Int) [firstParam]
        Right (pRest, moreParams)

  (p4, _)    <- parserConsume RIGHT_PAREN "Expect ')' after parameters." p3
  (p5, _)    <- parserConsume LEFT_BRACE ("Expect '{' before " <> kind <> " body.") p4
  (body, p6) <- stmtBlock p5

  Right (S_FUNCTION name params body, p6)

  where
    loop :: Parser -> Int -> [Token] -> Either BS.ByteString (Parser, [Token])
    loop pIn len acc
      | (True, pComma) <- parserMatch [COMMA] pIn
          = if len >= 255
              then
                Left $ loxError (t_line $ parserPeek pComma)
                  "Can't have more than 255 parameters."
              else do
                (pNext, nameTok) <- parserConsume IDENTIFIER "Expect parameter name." pComma
                loop pNext (len + 1) (nameTok : acc)

      | otherwise = Right (pIn, reverse acc)


stmtClassDeclaration :: Parser -> EitherStmtParser
stmtClassDeclaration p0 = do
  (p1, nameTok) <- parserConsume IDENTIFIER "Expect class name." p0

  let (ok, p2) = parserMatch [LESS] p1
  (p3, superclass) <-
      if ok
        then do
          (pSuper, superNameTok) <- parserConsume IDENTIFIER "Expect superclass name." p2
          Right (pSuper, Just (E_VARIABLE superNameTok))
        else
          Right (p2, Nothing)

  case superclass of
    Just (E_VARIABLE superTok)
      | t_lexeme superTok == t_lexeme nameTok ->
          Left $ loxError (t_line superTok) "A class can't inherit from itself."
    _ -> pure ()

  let pBrace = if ok then p3 else p2
  (p4, _) <- parserConsume LEFT_BRACE "Expect '{' before class body." pBrace

  let loop p acc =
        if parserCheck p RIGHT_BRACE || parserIsAtEnd p
          then Right (reverse acc, p)
          else do
            (method, p') <- stmtFunction p "method"
            loop p' (method : acc)

  (methods, p5) <- loop p4 []
  (p6, _) <- parserConsume RIGHT_BRACE "Expect '}' after class body." p5

  Right (S_CLASS nameTok superclass methods, p6)

stmtDeclaration :: Parser -> EitherStmtParser
stmtDeclaration p0 =
  case parserMatch [CLASS] p0 of
    (True, p1) -> stmtClassDeclaration p1
    _ ->
      case parserMatch [FUN] p0 of
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

