module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

import Types
import Environment



newInterpreterState :: InterpreterState
newInterpreterState = InterpreterState (mkRootEnv)

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
stringify (Number num) = fixedNum
  where fixedNum = if take 2 (reverse shownNum) == "0." then init . init $ shownNum else shownNum
        shownNum = show num

