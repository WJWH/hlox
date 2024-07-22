module Resolver where

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
resolveExpression (Grouping expr) = undefined
resolveExpression (Unary op expr) = undefined
resolveExpression (Binary op left right) = undefined
resolveExpression (Literal litContents) = undefined
resolveExpression (Variable tok) = undefined -- should call into resolveLocal
resolveExpression (Assignment tok expr) = undefined
resolveExpression (Logical op left right) = undefined
resolveExpression (Call calleeExpr tok argExprs) = undefined

-- puts an entry in the "locals" map if the variable can actually be found in one of
-- the scopes.
resolveLocal :: Expression -> String -> Resolver ()
resolveLocal expr varname = do
  -- find how far up the env ancestry we need to go to find the variable
  scopeStack <- gets scopes
  let varDepth = findDepth scopeStack varname
  -- then stick that information in the locals. If the variable couldn't be found then according
  -- to the book we assume it's a global that does not need to be resolved.
  case varDepth of
    Nothing -> return ()
    Just depth -> do
      oldLocals <- gets locals
      modify $ \s -> s { locals = M.insert expr depth oldLocals }

-- Begins variable definition process
declare :: String -> Resolver ()
declare varname = do
  scopeStack <- gets scopes
  case scopeStack of
    [] -> return ()
    (currentScope:parents) -> modify $ \s -> s { scopes = ((M.insert varname False currentScope) : parents)}

-- finishes variable definition process
define :: String -> Resolver ()
define varname = do
  scopeStack <- gets scopes
  case scopeStack of
    [] -> return ()
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

-- The book says that if the thing can't be found, we'll just assume the var is global so
-- maybe we need another value to indicate that
findDepth :: [Scope] -> String -> Maybe Int
findDepth [] _ = Nothing
findDepth (scope:parentScopes) varname = case M.lookup varname scope of
  Just True -> Just 0
  Just False -> Nothing -- we found it, but it's still being initialised.
  Nothing -> fmap (+1) $ findDepth parentScopes varname -- See if we can find it in one of the parent scopes
