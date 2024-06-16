module Main where

import Control.Monad.Reader
import Data.IORef
import System.Environment
import System.Exit
import System.IO
import Text.Parsec

import Scanner
import Parser

type HLox = ReaderT InterpreterContext IO
data InterpreterContext = Context { hadError :: IORef Bool } deriving (Eq) -- for now, to make it compile
instance Show InterpreterContext where
  show (Context _) = "some context"

emptyContext :: IO InterpreterContext
emptyContext = do
  ref <- newIORef False
  return $ Context ref

main :: IO ()
main = do
  args <- getArgs
  context <- emptyContext
  _ <- case args of
    [] -> runPrompt context
    (filename:[]) -> runFile filename
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitWith (ExitFailure 64)
  return ()

runFile :: FilePath -> IO InterpreterContext
runFile filepath = do
  contents <- readFile filepath
  context <- emptyContext
  finalContext <- runReaderT (run contents) context
  wasError <- readIORef $ hadError context
  case wasError of
    True -> exitWith (ExitFailure 65)
    False -> return finalContext

runPrompt :: InterpreterContext -> IO InterpreterContext
runPrompt context = do
  putStr "> "
  hFlush stdout
  nextLine <- getLine -- maybe catch isEOFError and turn into empty line?
  case nextLine of
    "" -> return context
    _ -> do
      newContext <- runReaderT (run nextLine) context
      resetErrors newContext
      runPrompt newContext

resetErrors :: InterpreterContext -> IO ()
resetErrors context = do
  let ref = hadError context
  writeIORef ref False

run :: String -> HLox InterpreterContext
run input = do
  let tokens = scanner 1 input
  let parseResult = parse expression "" tokens
  case parseResult of
    Left err -> do
      errorRef <- asks hadError
      liftIO $ writeIORef errorRef True
      liftIO $ print err
    Right expr -> do
      liftIO $ print expr
  ask >>= return


error :: Int -> String -> IO ()
error linenr message = report linenr "" message

-- according to the book this should also set a global "hadError" boolean to true
report :: Int -> String -> String -> IO ()
report linenr location message = do
  putStrLn $ concat ["[line ", show linenr, "] Error", location, ": ", message]
