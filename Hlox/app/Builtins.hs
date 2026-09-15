{-# LANGUAGE OverloadedStrings #-}
module Builtins where

import Types
import Helpers
import System.IO.Unsafe      (unsafePerformIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe            (fromMaybe)

import qualified Data.ByteString.Char8 as BS

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
            let out = loxValueShow lit
            in Right ([out], L_NIL, interp)

          _ -> Left "print() expects exactly 1 argument."
    }

builtinMin :: LoxCallable
builtinMin =
  LoxCallable
    { lc_arity = 2
    , lc_call  = \interp tok args ->
        case args of
          [L_NUMBER a, L_NUMBER b] -> Right ([], L_NUMBER (min a b), interp)
          _ -> Left $ loxError (t_line tok) "min() expects exactly 2 number arguments."
    }

builtinMax :: LoxCallable
builtinMax =
  LoxCallable
    { lc_arity = 2
    , lc_call  = \interp tok args ->
        case args of
          [L_NUMBER a, L_NUMBER b] -> Right ([], L_NUMBER (max a b), interp)
          _ -> Left $ loxError (t_line tok) "max() expects exactly 2 number arguments."
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

          _ -> Left $ loxError (t_line tok) "mod() expects exactly 2 number arguments."
    }

builtinToStr :: LoxCallable
builtinToStr =
  LoxCallable
    { lc_arity = 1
    , lc_call  = \interp tok args ->
        case args of
          [a] -> Right ([], L_STRING (loxValueShow a), interp)
          _ -> Left $ loxError (t_line tok) "string() expects exactly 1 literal argument."
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
            Left $ loxError (t_line tok) "double() expects exactly 1 string argument."
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
