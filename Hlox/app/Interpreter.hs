{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Types
import Builtins
import Helpers
import Environments
import Classes
import Scanner
import Parser

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as SM

execResultExtractInterp :: ExecResult -> Interpreter
execResultExtractInterp (ER_NORMAL i) = i
execResultExtractInterp (ER_BREAK i) = i
execResultExtractInterp (ER_CONTINUE i) = i
execResultExtractInterp (ER_RETURN _ i) = i

---------------------------------------------------------------------------------------
--                               Statement Evaluation
---------------------------------------------------------------------------------------

stmtExec :: Interpreter -> Stmt -> EitherListBsExecRes
stmtExec i0 (S_PRINT expr) = do
  (outsE, lit, i1) <- exprEval i0 expr
  let outs = outsE ++ [loxValueShow lit]
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

stmtExec interp0 (S_BLOCK stmts) = execBlock interp0 [] stmts

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
          env1 = envLoopEnter env0
      in interp0 { i_environment = env1 }

    loop interp accOuts = do
      (outsCond, lit, interp1) <- exprEval interp cond

      if not (exprIsTruthy lit)
        then
          let env1    = i_environment interp1
              env2    = envLoopExit env1
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
                  env3    = envLoopExit env2
                  interp3 = interp2 { i_environment = env3 }
              in Right (acc1, ER_NORMAL interp3)

            ER_RETURN litR interp2 ->
              Right (acc1, ER_RETURN litR interp2)

stmtExec interp0 (S_FOR initialize cond inc body) = do
  (outsInit, resInit) <-
    case initialize of
      Nothing   -> Right ([], ER_NORMAL interp0)
      Just stmt -> stmtExec interp0 stmt

  case resInit of
    ER_NORMAL interp1 ->
      let env1 = i_environment interp1
          env2 = envLoopEnter env1
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
              envE    = envLoopExit envC
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
                  envE    = envLoopExit envB
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
      env1 = envDefine env0 (t_lexeme name) (L_FUN fn)
      fn   = loxFunctionNew name params body env1 False
      interp1 = interp0 { i_environment = env1 }
  in Right ([], ER_NORMAL interp1)

stmtExec interp0 (S_CLASS name super methods) = do
  let env0 = i_environment interp0
      env1 = envDefine env0 (t_lexeme name) L_NIL
      interp1 = interp0 { i_environment = env1 }

  mSuper <- case super of
    Nothing -> pure Nothing

    Just (E_VARIABLE superTok) -> do
      (_, val, _) <- exprEval interp1 (E_VARIABLE superTok)
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
        let envSuper = envNew env1
            envSuper' = envDefine envSuper "super" (L_CLASS superClass)
            interpSuper = interp1 { i_environment = envSuper' }
        pure (envSuper', interpSuper)

  let methodMap =
        foldl'
          (\m stmt ->
             case stmt of
               S_FUNCTION fname params body ->
                 let isInit = t_lexeme fname == "init"
                     fn     = loxFunctionNew fname params body envForMethods isInit
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
    Left err -> Left err

    Right envFinal ->
      let interpFinal' = interpFinal { i_environment = envFinal }
      in pure ([], ER_NORMAL interpFinal')

stmtExec interp (S_RETURN _ Nothing) = Right ([], ER_RETURN L_NIL interp)

stmtExec interp0 (S_RETURN _ (Just expr)) = do
  (bs, val, interp1) <- exprEval interp0 expr
  Right (bs, ER_RETURN val interp1)

-------------------------------------------------------------------------------------------
--                                 Expression Evaluation 
-------------------------------------------------------------------------------------------

exprEval :: Interpreter -> Expr -> EitherListBsLoxValueInterp
exprEval i (E_LITERAL (Just lit)) = Right ([], lit, i)
exprEval i (E_LITERAL Nothing) = Right ([], L_NIL, i)
exprEval i (E_GROUPING e) = exprEval i e

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

    _ -> error "Internal error: E_LOGICAL with non-logical operator"

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

           _ -> Right (outs1 ++ outs2, L_INSTANCE inst', interp2)

    _ -> Left (loxError (t_line nameTok) "Only instances have fields.")

exprEval interp (E_THIS tok) = do
  let env = i_environment interp
  case envGet env tok of
    Right lit -> Right ([], lit, interp)
    Left _    -> Left (loxError (t_line tok) "Can't use 'this' outside of a class.")

exprEval interp0 (E_SUPER superTok methodTok) = do
  let env = i_environment interp0

  superVal <- case envGet env superTok of
          Right v -> pure v
          Left _  ->
              Left (loxError (t_line superTok)
                  "Cannot use 'super' here: no superclass in this context.")

  klass    <- case superVal of
    L_CLASS k -> pure k
    _         -> Left (loxError (t_line superTok) "'super' is not a class")

  let tokThis = Token
        { t_type    = IDENTIFIER
        , t_lexeme  = "this"
        , t_loxValue = Nothing
        , t_line    = t_line superTok
        }

  thisVal <- envGet env tokThis
  inst <- case thisVal of
    L_INSTANCE i -> pure i
    _            -> Left (loxError (t_line superTok) "'this' is not an instance")

  let methodName = t_lexeme methodTok
  case findMethod klass methodName of
    Nothing ->
      Left (loxError (t_line methodTok) ("Undefined property '" <> methodName <> "'"))
    Just fn -> do
      let bound = bindThis fn inst
      pure ([], L_FUN bound, interp0)

-------------------------------------------------------------------------------------------
--                                   Interpreter 
-------------------------------------------------------------------------------------------

interpreterInit :: Interpreter
interpreterInit =
  let
    g0 = envInit
    g1 = foldl' (\env (name, fn) -> envDefine env name (L_CALL fn)) g0 builtIns
  in
    Interpreter { i_globals = g1, i_environment = g1 }

loxFunctionNew :: Token -> [Token] -> [Stmt] -> Env -> Bool -> LoxFunction
loxFunctionNew name params body closure isInit =
  LoxFunction { lf_name = name
              , lf_params = params
              , lf_body = body
              , lf_closure = closure
              , lf_initializer = isInit
              }

execBlock :: Interpreter -> [BS.ByteString] -> [Stmt] -> EitherListBsExecRes
execBlock interp0 = go interp1
  where
    env0  = i_environment interp0
    env1  = envNew env0
    interp1 = interp0 { i_environment = env1 }

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
        ER_NORMAL interp'   -> go interp' acc' ss
        ER_BREAK  interp'   -> Right (acc', ER_BREAK interp')
        ER_CONTINUE interp' -> Right (acc', ER_CONTINUE interp')
        ER_RETURN lit interp' -> Right (acc', ER_RETURN lit interp')

callUserFunction :: LoxFunction -> Interpreter -> Token -> [LoxValue] -> EitherListBsLoxValueInterp
callUserFunction fn interp0 _ args = do
  let callerEnv = i_environment interp0
      closure   = lf_closure fn
      params    = lf_params fn
      body      = lf_body fn
      isInit    = lf_initializer fn
      callEnv0  = envNew closure
      callEnv1  =
        foldl'
          (\env (paramTok, argVal) -> envDefine env (t_lexeme paramTok) argVal)
          callEnv0
          (zip params args)

      interp1 = interp0 { i_environment = callEnv1 }

  (outs, res) <- execBlock interp1 [] body

  let restore interpAfter = interpAfter { i_environment = callerEnv }

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

classCallable :: LoxClass -> LoxCallable
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

expectCallable :: LoxValue -> EitherLoxCallable
expectCallable (L_CALL c)  = Right c
expectCallable (L_FUN f)   = Right (wrapUserFunction f)
expectCallable (L_CLASS k) = Right (classCallable k)
expectCallable _           = Left "Can only call functions and classes."

interpRun :: Interpreter -> String -> IO (Interpreter, [BS.ByteString])
interpRun interp source = do
  let sc0 = scannerInit source 1
      sc1 = scannerRun sc0

  case s_errors sc1 of
    errs@(_:_) -> pure (interp, errs)

    [] -> do
      let parser0            = createParser (s_tokens sc1)
          (outs, _, interp1) = loop interp parser0
      pure (interp1, outs)

  where
    loop :: Interpreter -> Parser -> ([BS.ByteString], Parser, Interpreter)
    loop env p0 = go env p0 []
      where
        go env' p acc
          | parserIsAtEnd p = (acc, p, env')
          | otherwise =
              case stmtDeclaration p of
                Left err ->
                  let pSync = synchronize p
                  in (acc ++ [err], pSync, env')

                Right (stmt, p1) ->
                  case stmtExec env' stmt of
                    Left err -> (acc ++ [err], p1, env')

                    Right (outs, res) ->
                      let env'' = execResultExtractInterp res
                      in go env'' p1 (acc ++ outs)

