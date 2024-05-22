{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit

data InterpreterContext = Context { hadError :: Bool } deriving (Show,Eq) -- for now, to make it compile
emptyContext = Context False

main :: IO ()
main = do
  args <- getArgs
  _ <- case args of
    [] -> runPrompt emptyContext
    (filename:[]) -> runFile filename
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitWith (ExitFailure 64)
  return ()

runFile :: FilePath -> IO InterpreterContext
runFile filepath = do
  contents <- TIO.readFile filepath
  finalContext <- run emptyContext contents
  case hadError finalContext of
    True -> exitWith (ExitFailure 65)
    False -> return finalContext

runPrompt :: InterpreterContext -> IO InterpreterContext
runPrompt context = do
  putStr "> "
  nextLine <- TIO.getLine
  case nextLine of
    "" -> return context
    _ -> do
      newContext <- run context nextLine
      resetErrors
      runPrompt newContext

resetErrors = undefined

run :: InterpreterContext -> T.Text -> IO InterpreterContext
run context input = do
  tokens <- scanner input
  mapM_ print tokens
  return context


error :: Int -> String -> IO ()
error linenr message = report linenr "" message

-- according to the book this should also set a global "hadError" boolean to true
report :: Int -> String -> String -> IO ()
report linenr location message = do
  putStrLn $ concat ["[line ", show linenr, "] Error", location, ": ", message]

scanner = undefined