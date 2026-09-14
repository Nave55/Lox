{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Interpreter
import System.IO          (hFlush, stdout)
import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as BS

runFile :: FilePath -> IO ()
runFile path = do
  bytes     <- readFile path
  (_, outs) <- interpRun interpreterInit bytes
  mapM_ BS.putStrLn outs

runRepl :: IO ()
runRepl = do
  putStrLn "-----------------------------------------"
  putStrLn "                 LOX REPL                "
  putStrLn "-----------------------------------------"
  loop interpreterInit
  where
    loop interp = do
      putStr ">>> "
      hFlush stdout
      line <- getLine

      if line == ":q" || line == "exit"
        then pure ()
        else do
          (interp', outs) <- interpRun interp line
          mapM_ BS.putStrLn outs
          loop interp'

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> runRepl
    [p] -> runFile p
    _   -> putStrLn "Too many args. More than 1 arg not permitted."
