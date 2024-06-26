module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

import Types
import Environment



newInterpreterState :: InterpreterState
newInterpreterState = InterpreterState globals globals
  where globals = mkRootEnv

-- and then:
runInterpreter :: InterpreterState -> Interpreter a -> IO (Either InterpreterError a, InterpreterState)
runInterpreter st i = runStateT (runExceptT i) st

interpret :: InterpreterState -> [Statement] -> IO InterpreterState
interpret startState stmts = do
  (result, finalState) <- runInterpreter startState (executeMany stmts)
  either (\err -> print err >> return startState) (\_res -> return finalState) result

executeMany :: [Statement] -> Interpreter ()
executeMany stmts = sequence_ $ map execute stmts

execute :: Statement -> Interpreter ()
execute (ExprStatement expr) = evaluate expr >> return ()
execute (PrintStatement expr) = do
  exprVal <- evaluate expr
  liftIO . putStrLn $ stringify exprVal
execute (VariableDeclaration varName maybeExpr) = do
  case maybeExpr of
    Nothing -> defineVar varName Null
    Just expr -> do
      exprVal <- evaluate expr
      defineVar varName exprVal
execute (Block stmts) = do
  currentState <- get
  -- the interpreter state for the block is the same as for the parent scope, but with a fresh child env
  let blockState = currentState { env = mkChildEnv (env currentState) }
  -- run all the statements with the new state. If there's an error, just reraise it
  -- as no state has been changed yet. If there is no error, we recover the parent env
  -- from the returned state as some of the variables in the outer scopes may have been
  -- assigned to.
  (result, finalState) <- liftIO $ runInterpreter blockState (executeMany stmts)
  either (\err -> throwError err)
         (\_res -> do
                    let finalEnv = env finalState
                    -- fromJust is safe here because it is guaranteed that there is a parent env
                    put $ currentState { env = fromJust $ parent finalEnv }
         )
         result
execute (IfStatement condition thenBranch elseBranch) = do
  condVal <- evaluate condition
  if isTruthy condVal
    then execute thenBranch
    else case elseBranch of
      Nothing -> return ()
      Just stmt -> execute stmt
execute stmt@(WhileStatement condition body) = do
  condVal <- evaluate condition
  when (isTruthy condVal) (execute body >> execute stmt) -- yay tail call optimization
execute EmptyStatement = return () -- basically a NOP, only used for for loops
execute (FunctionDeclaration name args body) = do
  let fn = LoxFunction (length args) name args body
  defineVar name fn
execute (ReturnStatement expr) = do
  exprVal <- evaluate expr
  throwError $ ReturnValue exprVal

evaluate :: Expression -> Interpreter RuntimeValue
evaluate (Literal (NumberLit num)) = return $ Number num
evaluate (Literal (StringLit str)) = return $ String str
evaluate (Literal TrueLit) = return $ Boolean True
evaluate (Literal FalseLit) = return $ Boolean False
evaluate (Literal NilLit) = return $ Null
evaluate (Grouping expr) = evaluate expr
-- Can only be numerical negation (for numbers only obv) or logical negation
evaluate (Unary op expr) = case op of
  Negate -> do
    exprVal <- evaluate expr
    case exprVal of
      Number num -> return . Number $ negate num
      _ -> throwError $ ArgumentError "Can only negate numbers"
  Bang -> do
    exprVal <- evaluate expr
    return $ if isTruthy exprVal then Boolean False else Boolean True
-- Binary operators usually need their operators to be either two numbers (for +-*/) or
-- two Booleans (for <>== etc), except for `+` which can take two numbers but also two strings.
-- In all cases we evaluate both sides first
evaluate (Binary op left right) = do
  leftVal <- evaluate left
  rightVal <- evaluate right
  case op of
    DoubleEqual -> return . Boolean $ leftVal == rightVal
    NotEqual -> return . Boolean $ leftVal /= rightVal
    LessThan -> do
      ensureBothNumber LessThan leftVal rightVal
      return . Boolean $ (numVal leftVal) < (numVal rightVal)
    LessEqualThan -> do
      ensureBothNumber LessEqualThan leftVal rightVal
      return . Boolean $ (numVal leftVal) <= (numVal rightVal)
    GreaterThan -> do
      ensureBothNumber GreaterThan leftVal rightVal
      return . Boolean $ (numVal leftVal) > (numVal rightVal)
    GreaterEqualThan -> do
      ensureBothNumber GreaterEqualThan leftVal rightVal
      return . Boolean $ (numVal leftVal) >= (numVal rightVal)
    Subtract -> do
      ensureBothNumber Subtract leftVal rightVal
      return . Number $ (numVal leftVal) - (numVal rightVal)
    Multiply -> do
      ensureBothNumber Multiply leftVal rightVal
      return . Number $ (numVal leftVal) * (numVal rightVal)
    Divide -> do
      ensureBothNumber Divide leftVal rightVal
      return . Number $ (numVal leftVal) / (numVal rightVal)
    Add -> case (leftVal, rightVal) of
      (Number l, Number r) -> return . Number $ l + r
      (String ll, String rr) -> return . String $ ll ++ rr
      _ -> throwError $ ArgumentError ("Arguments to Plus must be either two Numbers or two Strings")
evaluate (Variable name) = getVar name
evaluate (Assignment name expr) = do
  exprVal <- evaluate expr
  assignVar name exprVal
evaluate (Logical op left right) = do
  leftVal <- evaluate left
  case op of
    Or -> if isTruthy leftVal then return leftVal else evaluate right
    And -> if not . isTruthy $ leftVal then return leftVal else evaluate right
evaluate (Call callee _tok args) = do
  calleeVal <- evaluate callee
  argsVals <- mapM evaluate args
  case calleeVal of
    nf@(NativeFunction _ _ _) -> call nf argsVals
    lf@(LoxFunction _ _ _ _) -> call lf argsVals
    _ -> throwError $ RuntimeError "Can only call functions and classes."

call :: RuntimeValue -> [RuntimeValue] -> Interpreter RuntimeValue
call (NativeFunction _arity _name code) args = do
  code args -- but how about the args?? Will I need a separate one for that?
  -- idea: all the args they take MUST be RuntimeValues, so perhaps I can make them all
  -- take a single argument of type [RuntimeValue]?
call (LoxFunction arity name argNames body) args = do
  when (arity /= length args) $ throwError $ RuntimeError ("wrong arity for function " ++ name)
  currentState <- get
  -- the interpreter state for the block is the same as for the parent scope, but with a fresh child env
  let functionState = currentState { env = mkChildEnv (env currentState) }
  -- run all the statements with the new state. If there's an error, just reraise it
  -- as no state has been changed yet. If there is no error, we recover the parent env
  -- from the returned state as some of the variables in the outer scopes may have been
  -- assigned to.
  (result, finalState) <- liftIO $ runInterpreter functionState $ do
    mapM_ (\(argName, a) -> defineVar argName a) (zip argNames args) -- assign the params
    execute body -- run the body in this new env with the params defined
  either (\err -> case err of
                    ReturnValue val -> return val
                    _ -> throwError err
         )
         (\_res -> do
                    let finalEnv = env finalState
                    -- fromJust is safe here because it is guaranteed that there is a parent env
                    put $ currentState { env = fromJust $ parent finalEnv }
                    -- the case where a value is returned was handled above in the error branch, but
                    -- if the function completed without any return value we still have to return
                    -- something, in this case Null
                    return Null
         )
         result
call _ _ = throwError $ RuntimeError "Called 'call' with non-function argument (should be impossible)"

-- Utility functions
isTruthy :: RuntimeValue -> Bool
isTruthy Null = False
isTruthy (Boolean False) = False
isTruthy _ = True

numVal :: RuntimeValue -> Double
numVal (Number num) = num
numVal _ = error "Unreachable, tried to call numVal on non-number runtime value"

-- Will throw an exception unless both values are Numbers
ensureBothNumber :: BinaryOperation -> RuntimeValue -> RuntimeValue -> Interpreter ()
ensureBothNumber _ (Number _) (Number _) = return ()
ensureBothNumber op _ _ = throwError $ ArgumentError ("Both arguments to " ++ show op ++ " must be Numbers")

stringify :: RuntimeValue -> String
stringify (String str) = str
stringify Null = "nil"
stringify (Boolean b) = show b
stringify (NativeFunction _ name _) = "native function: " ++ name
stringify (LoxFunction _ name _ _) = "function: " ++ name
stringify (Number num) = fixedNum
  where fixedNum = if take 2 (reverse shownNum) == "0." then init . init $ shownNum else shownNum
        shownNum = show num
