{-# LANGUAGE OverloadedStrings #-}

module Classes where

import Types
import Environments
import Helpers

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as SM

bindThis :: LoxFunction -> LoxInstance -> LoxFunction
bindThis fn inst =
  let closure = lf_closure fn
      env0    = envNew closure
      env1    = envDefine env0 "this" (L_INSTANCE inst)
  in fn { lf_closure = env1 }

instanceGet :: LoxInstance -> Token -> Either BS.ByteString LoxValue
instanceGet inst nameTok =
  case SM.lookup (t_lexeme nameTok) (li_fields inst) of
    Just v  -> Right v
    Nothing ->
      case findMethod (li_class inst) (t_lexeme nameTok) of
        Just fn -> Right (L_FUN (bindThis fn inst))
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
