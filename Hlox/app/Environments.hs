{-# LANGUAGE OverloadedStrings #-}
module Environments where

import Types
import Helpers

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as SM

envInit :: Env
envInit = Env
  { e_map        = SM.empty
  , e_enclosing  = Nothing
  , e_loop_depth = 0
  }

envNew :: Env -> Env
envNew parent = Env
  { e_map        = SM.empty
  , e_enclosing  = Just parent
  , e_loop_depth = e_loop_depth parent
  }


envLoopEnter :: Env -> Env
envLoopEnter env = env { e_loop_depth = e_loop_depth env + 1 }

envLoopExit :: Env -> Env
envLoopExit env = env { e_loop_depth = e_loop_depth env - 1 }

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

envGet :: Env -> Token -> EitherLoxValue
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
