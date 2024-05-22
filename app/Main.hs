{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit

data InterpreterContext = NoContext deriving (Show,Eq) -- for now, to make it compile

main :: IO ()
main = do
  args <- getArgs
  _ <- case args of
    [] -> runPrompt NoContext
    (filename:[]) -> runFile filename
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitWith (ExitFailure 64)
  return ()

runFile :: FilePath -> IO InterpreterContext
runFile filepath = do
  contents <- TIO.readFile filepath
  run NoContext contents

runPrompt :: InterpreterContext -> IO InterpreterContext
runPrompt context = do
  putStr "> "
  nextLine <- TIO.getLine
  case nextLine of
    "" -> return context
    _ -> do
      newContext <- run context nextLine
      runPrompt newContext


run :: InterpreterContext -> T.Text -> IO InterpreterContext
run = undefined
