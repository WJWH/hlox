module Resolver where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

import Types

resolve :: Locals -> [Statement] -> IO Locals
resolve locals stmts = undefined

runResolver :: ResolverState -> Resolver a -> IO (Either ResolverError a, ResolverState)
runResolver st r = runStateT (runExceptT r) st

-- these either add to the Locals in the Resolver state, recurse to child AST nodes or both
resolveStatement :: Statement -> Resolver ()
resolveStatement (Block stmts) = do
  beginScope
  mapM_ resolveStatement stmts
  endScope
resolveStatement (VariableDeclaration token maybeInitializer) = do
  declare $ lexeme token
  case maybeInitializer of
    Nothing -> return ()
    Just expr -> resolveExpression expr
  define $ lexeme token

resolveExpression :: Expression -> Resolver ()
resolveExpression = undefined

resolveLocal :: Expression -> String -> Resolver ()
resolveLocal = undefined

-- Begins variable definition process
declare :: String -> Resolver ()
declare varname = do
  scopeStack <- gets scopes
  case scopeStack of
    [] -> throwError (ResolverError "No scope declared")
    (currentScope:parents) -> modify $ \s -> s { scopes = ((M.insert varname False currentScope) : parents)}

-- finishes variable definition process
define :: String -> Resolver ()
define varname = do
  scopeStack <- gets scopes
  case scopeStack of
    [] -> throwError (ResolverError "No scope declared")
    (currentScope:parents) -> modify $ \s -> s { scopes = ((M.insert varname True currentScope) : parents)}

-- utility functions
beginScope :: Resolver ()
beginScope = do
  scopeStack <- gets scopes
  modify $ \s -> s { scopes = (M.empty : scopeStack) }

endScope :: Resolver ()
endScope = do
  scopeStack <- gets scopes
  modify $ \s -> s { scopes = drop 1 scopeStack }
