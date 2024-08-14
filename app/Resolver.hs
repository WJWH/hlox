module Resolver where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import Types

resolve :: Locals -> [Statement] -> IO (Either ResolverError Locals)
resolve previousLocals stmts = do
  result <- runResolver initialState $ mapM_ resolveStatement stmts
  case result of
    (Right _, finalState) -> return $ Right $ resolverLocals finalState
    (Left err, _finalState) -> return $ Left err
  where initialState = ResolverState [] previousLocals None -- no scopes, locals from args and not in any function

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

resolveStatement (ExprStatement expr) = resolveExpression expr
resolveStatement (PrintStatement expr) = resolveExpression expr
resolveStatement (FunctionDeclaration tok params body) = do
  enclosingFunction <- gets currentFunction -- stash the current function type in this var for now
  modify $ \s -> s { currentFunction = Function } -- then update current function type to "Function"
  declare $ lexeme tok
  define $ lexeme tok
  beginScope
  mapM_ (\param -> declare (lexeme param) >> define (lexeme param)) params
  resolveStatement body
  endScope
  modify $ \s -> s { currentFunction = enclosingFunction } -- restore old value
resolveStatement (IfStatement condition trueBranch falseBranch) = do
  resolveExpression condition
  resolveStatement trueBranch
  when (isJust falseBranch) $ resolveStatement (fromJust falseBranch)
resolveStatement (WhileStatement condition body) = do
  resolveExpression condition
  resolveStatement body
resolveStatement (ReturnStatement expr) = do
  currentFunctionType <- gets currentFunction
  case currentFunctionType of
    None -> throwError . ResolverError $ "Can't return from top-level code." -- can only `return` from functions
    Function -> resolveExpression expr
resolveStatement (ClassDeclaration nameToken _methods) = do
  declare (lexeme nameToken)
  define (lexeme nameToken)
resolveStatement (EmptyStatement) = return ()

resolveExpression :: Expression -> Resolver ()
-- these expression types reference variables directly and should call into resolveLocal
resolveExpression (Variable tok) = do
  scopeStack <- gets scopes
  -- IF there is a current scope AND the variable is in there BUT its still False, then we need
  -- to error because this variable is being used in its own initializer, which is an error
  case scopeStack of
    [] -> return () -- no current scope exists, do nothing
    (currentScope:_parentScopes) -> case M.lookup (lexeme tok) currentScope of
      Just False -> throwError $ ResolverError "Can't read local variable in its own initializer."
      _ -> return () -- Both Just True and Nothing are OK, do nothing
  resolveLocal (Variable tok) (lexeme tok)

resolveExpression (Assignment tok expr) = do
  resolveExpression expr
  resolveLocal (Assignment tok expr) (lexeme tok)
-- all other expressions can just recurse into their subcomponents
resolveExpression (Grouping expr) = resolveExpression expr
resolveExpression (Unary _op expr) = resolveExpression expr
resolveExpression (Binary _op left right) = resolveExpression left >> resolveExpression right
resolveExpression (Literal _litContents) = return () -- nothing to do here, a literal is by definition not a variable
resolveExpression (Logical _op left right) = resolveExpression left >> resolveExpression right
resolveExpression (Call calleeExpr _tok argExprs) = do
  resolveExpression calleeExpr
  mapM_ resolveExpression argExprs

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
      oldLocals <- gets resolverLocals
      modify $ \s -> s { resolverLocals = M.insert expr depth oldLocals }

-- Begins variable definition process
declare :: String -> Resolver ()
declare varname = do
  scopeStack <- gets scopes
  case scopeStack of
    [] -> return ()
    (currentScope:parents) -> do
      case M.lookup varname currentScope of
        Nothing -> modify $ \s -> s { scopes = ((M.insert varname False currentScope) : parents)}
        Just _ -> throwError . ResolverError $ "Already a variable with this name in this scope."

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
