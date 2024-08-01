module Main where

import qualified Data.Map as M
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding (tokens)

import Interpreter
import Parser
import Resolver
import Scanner
import Types

data InterpreterContext = Context { hadError :: Bool, interpreterState :: InterpreterState } deriving (Eq) -- for now, to make it compile
instance Show InterpreterContext where
  show (Context _ _) = "some context"

emptyContext :: IO InterpreterContext
emptyContext = do
  newIS <- newInterpreterState M.empty
  return $ Context False newIS

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
  finalContext <- run context contents
  let wasError = hadError context
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
      newContext <- run context nextLine
      let resetContext = resetErrors newContext
      runPrompt resetContext

resetErrors :: InterpreterContext -> InterpreterContext
resetErrors context = context { hadError = True }

run :: InterpreterContext -> String -> IO InterpreterContext
run context input = do
  let tokens = scanner 1 0 input
  let parseResult = parse program "" tokens
  case parseResult of
    Left err -> do
      print err
      return $ context { hadError = True }
    Right stmts -> do
      resolverResult <- resolve M.empty stmts
      case resolverResult of
        Left err -> print err >> return context
        Right newlocals -> do
          let startState = (interpreterState context) { locals = newlocals }
          newState <- interpret startState stmts
          return $ context { interpreterState = newState }

error :: Int -> String -> IO ()
error linenr message = report linenr "" message

-- according to the book this should also set a global "hadError" boolean to true
report :: Int -> String -> String -> IO ()
report linenr location message = do
  putStrLn $ concat ["[line ", show linenr, "] Error", location, ": ", message]
