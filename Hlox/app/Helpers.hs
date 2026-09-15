{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Types
import Numeric   (showFFloat)
import Data.List (dropWhileEnd)

import qualified Data.ByteString.Char8 as BS

loxReport :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
loxReport line loc msg =
  "[line " <> BS.pack (show line) <> "] Error " <> loc <> "-> " <> msg

loxError :: Int -> BS.ByteString -> BS.ByteString
loxError line = loxReport line ""

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
formatNum n l = BS.pack (stripDotZero (showFFloat Nothing (round' n l) ""))
  where
    stripDotZero s =
      case break (== '.') s of
        (int, "") ->
          int

        (int, '.':frac) ->
          if all (== '0') frac
            then int
            else int ++ "." ++ dropWhileEnd (== '0') frac

        _ -> s

    round' num sg =
      let f       = 10 ** fromIntegral sg
          rounded :: Integer
          rounded = round (num * f)
      in fromIntegral rounded / f

loxValueShow :: LoxValue -> BS.ByteString
loxValueShow (L_NUMBER n)      = formatNum n 10
loxValueShow (L_STRING s)      = s
loxValueShow (L_BOOL b)        = if b then "true" else "false"
loxValueShow L_NIL             = "nil"
loxValueShow (L_FUN _)         = "<fn>"
loxValueShow (L_CALL _)        = "<native fn>"
loxValueShow (L_CLASS klass)   = lc_name klass
loxValueShow (L_INSTANCE inst) = BS.pack (show inst)
