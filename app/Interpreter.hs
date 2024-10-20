module Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.IORef
import Data.List (foldl')

import Types
import Environment



newInterpreterState :: Locals -> IO InterpreterState
newInterpreterState locals = do
  globals <- mkRootEnv
  return $ InterpreterState globals globals locals

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
    Nothing -> defineVar (lexeme varName) Null
    Just expr -> do
      exprVal <- evaluate expr
      defineVar (lexeme varName) exprVal
execute (Block stmts) = do
  currentState <- get
  -- the interpreter state for the block is the same as for the parent scope, but with a fresh child env
  blockEnv <- mkChildEnv (env currentState)
  let blockState = currentState { env = blockEnv }
  -- run all the statements with the new state.
  (result, _finalState) <- liftIO $ runInterpreter blockState (executeMany stmts)
  either (\err -> throwError err)
         (\_res -> return ())
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
execute (FunctionDeclaration nameToken args body) = do
  closure <- gets env
  -- By the time we get here, keeping the entire token is no longer required I think?
  -- "bare" functions are never initializers
  let fn = LoxFunction (length args) (lexeme nameToken) (map lexeme args) body closure False
  defineVar (lexeme nameToken) fn
execute (ClassDeclaration nameToken maybeSuperclass methods) = do
  closure <- gets env -- all methods share the same env I think?
  let classMethods = foldl' (\ms method -> methodDefine closure method ms) M.empty methods
  superclass <- case maybeSuperclass of
    Nothing -> return $ Nothing
    Just expr -> do
      superObject <- evaluate expr
      case superObject of
        cl@(LoxClass _ _ _) -> return $ Just cl
        _ -> throwError $ ArgumentError "Superclass must be a class!"
  let klass = LoxClass (lexeme nameToken) superclass classMethods
  defineVar (lexeme nameToken) klass
execute (ReturnStatement maybeExpr) = do
  case maybeExpr of
    Nothing -> throwError $ ReturnValue Null
    Just expr -> do
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
evaluate expr@(Variable nameToken) = lookupVariable nameToken expr
evaluate ass@(Assignment nameToken expr) = do
  exprVal <- evaluate expr
  assignVariable ass nameToken exprVal
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
    lf@(LoxFunction _ _ _ _ _ _) -> call lf argsVals
    cl@(LoxClass _ _ _) -> call cl argsVals
    _ -> throwError $ RuntimeError "Can only call functions and classes."
evaluate (Get callee (Variable property) _tok) = do
  object <- evaluate callee
  case object of
    loxInstance@(LoxInstance klass fieldsRef) -> do
      -- first we check if the property is a field
      fields <- liftIO $ readIORef fieldsRef
      case M.lookup (lexeme property) fields of
        -- if we find a field with that name, just return that:
        Just val -> return val
        -- perhaps it was not a field, maybe it's a method?
        -- Nothing -> case M.lookup (lexeme property) (classMethods klass) of
        Nothing -> case findMethod (lexeme property) klass of
          Just method -> bind loxInstance method
          Nothing -> throwError $ RuntimeError $ concat ["Undefined property ", lexeme property, "."]
    _ -> throwError $ RuntimeError "Only instances have fields."
evaluate (Get _ _ _) = do
  throwError $ RuntimeError "Should never happen: get expression was called with a non-variable property value."
evaluate (Set callee (Variable property) _tok valueExpr) = do
  object <- evaluate callee
  case object of
    LoxInstance _ fieldsRef -> do
      value <- evaluate valueExpr
      liftIO $ modifyIORef' fieldsRef $ M.insert (lexeme property) value
      return value
    _ -> throwError $ RuntimeError "Only instances have fields."
evaluate (Set _ _ _ _) = do
  throwError $ RuntimeError "Should never happen: set expression was called with a non-variable property value."
evaluate expr@(This nameToken) = lookupVariable nameToken expr

call :: RuntimeValue -> [RuntimeValue] -> Interpreter RuntimeValue
call (NativeFunction _arity _name code) args = do
  code args -- but how about the args?? Will I need a separate one for that?
  -- idea: all the args they take MUST be RuntimeValues, so perhaps I can make them all
  -- take a single argument of type [RuntimeValue]?
call (LoxFunction arity name argNames body closure isInitializer) args = do
  when (arity /= length args) $ throwError $ RuntimeError ("wrong arity for function " ++ name)
  currentState <- get
  -- the interpreter state for the block is the same as for the parent scope, but with a fresh child env
  -- that has the closure of the function as its parent
  childEnv <- mkChildEnv closure
  let functionState = currentState { env = childEnv } -- from the book but doesn't work??
  -- I think it is because you actually do need mutable references, otherwise the changes to one "global"
  -- parent scope won't show up in the other :|
  -- let functionState = currentState { env = mkChildEnv (env currentState) }
  -- run all the statements with the new state. If there's an error, just reraise it
  -- as no state has been changed yet. If there is no error, we recover the parent env
  -- from the returned state as some of the variables in the outer scopes may have been
  -- assigned to.
  (result, _finalState) <- liftIO $ runInterpreter functionState $ do
    mapM_ (\(argName, a) -> defineVar argName a) (zip argNames args) -- assign the params
    execute body -- run the body in this new env with the params defined
  either (\err -> case err of
                    ReturnValue val -> if isInitializer
                      then do
                        maybeThis <- findVar "this" closure
                        case maybeThis of
                          Just this -> return this
                          Nothing -> throwError $ RuntimeError "Should never happen: Could not find `this` variable in initializer env."
                      else return val
                    _ -> throwError err
         )
         (\_res -> do
                    -- the case where a value is returned was handled above in the error branch, but
                    -- if the function completed without any return value we still have to return
                    -- something, in this case Null. Special case: an initializer always returns `this`
                    -- instead of Null
                    if isInitializer
                      then do
                        maybeThis <- findVar "this" closure
                        case maybeThis of
                          Just this -> return this
                          Nothing -> throwError $ RuntimeError "Should never happen: Could not find `this` variable in initializer env."
                      else return Null
         )
         result
call cl@(LoxClass name _superclass methods) args = do
  fieldsRef <- liftIO $ newIORef M.empty
  -- does the class have an initializer?
  case M.lookup "init" methods of
    Nothing -> do
      when (length args > 0) $ throwError $ RuntimeError ("wrong arity for class instantiation of class " ++ name)
      return $ LoxInstance cl fieldsRef -- fields start out empty if there is no initializer
    Just initializer@(LoxFunction arity _ _ _ _ _) -> do
      when (length args /= arity) $ throwError $ RuntimeError ("wrong arity for class instantiation of class " ++ name)
      let newInstance = LoxInstance cl fieldsRef
      boundInitializer <- bind newInstance initializer -- so that `this` works inside the initializer
      void $ call boundInitializer args -- might mutate newInstance
      return newInstance
    _ -> error "Should never happen: initializer of a class was somehow not a function"
call _ _ = throwError $ RuntimeError "Called 'call' with non-function argument (should be impossible)"

-- Utility functions
isTruthy :: RuntimeValue -> Bool
isTruthy Null = False
isTruthy (Boolean False) = False
isTruthy _ = True

numVal :: RuntimeValue -> Double
numVal (Number num) = num
numVal _ = error "Unreachable, tried to call numVal on non-number runtime value"

-- find the method in the given class or any of its superclasses
findMethod :: String -> RuntimeValue -> Maybe RuntimeValue
findMethod name (LoxClass _ maybeSuperclass methods) = case M.lookup name methods of
  Just method -> Just method
  Nothing -> case maybeSuperclass of
    Nothing -> Nothing
    Just superclass -> findMethod name superclass
findMethod _ _ = error "Should never happen: findMethod was called on a non-class RuntimeValue"

-- define methods during class declarations
methodDefine :: Env -> Statement -> M.Map String RuntimeValue -> M.Map String RuntimeValue
methodDefine env (FunctionDeclaration nameToken args stmt) ms  = M.insert name newFunction ms
  where newFunction = LoxFunction (length args) name (map lexeme args) stmt env isInitializer
        isInitializer = name == "init"
        name = lexeme nameToken
methodDefine _ _ _ = error "Should never happen: methodDefine called with a non-method argument"

-- bind method to make 'this' keyword functional
-- actual type is bind :: LoxInstance -> LoxFunction -> Interpreter LoxFunction
-- but we use the following because I would need more advanced type trickery
bind :: RuntimeValue -> RuntimeValue -> Interpreter RuntimeValue
bind loxInstance@(LoxInstance _ _) (LoxFunction arity name args stmt closure isInitializer) = do
  newClosure <- mkChildEnv closure
  defineVarRaw "this" loxInstance newClosure
  return (LoxFunction arity name args stmt newClosure isInitializer)
bind _ _ = error "Should never happen: bind called with wrong argument types"

lookupVariable :: Token -> Expression -> Interpreter RuntimeValue
lookupVariable nameToken expr = do
  localVars <- gets locals
  case M.lookup expr localVars of
    Nothing -> do
      globalVars <- gets globals
      findResult <- findVar (lexeme nameToken) globalVars
      case findResult of
        Nothing -> throwError . RuntimeError $ "Undefined variable read: '" ++ (lexeme nameToken) ++ "'."
        Just var -> return var
    Just depth -> do
      getVarAt depth (lexeme nameToken)

-- assignVar (lexeme nameToken) exprVal
assignVariable :: Expression -> Token -> RuntimeValue -> Interpreter RuntimeValue
assignVariable ass nameToken exprVal = do
  localVars <- gets locals
  case M.lookup ass localVars of
    Nothing -> do
      globalVars <- gets globals
      setResult <- setVar (lexeme nameToken) exprVal globalVars
      case setResult of
        Nothing -> throwError . RuntimeError $ "Undefined variable write: '" ++ (lexeme nameToken) ++ "'."
        Just _var -> return exprVal
    Just depth -> do
      setVarAt depth (lexeme nameToken) exprVal


-- Will throw an exception unless both values are Numbers
ensureBothNumber :: BinaryOperation -> RuntimeValue -> RuntimeValue -> Interpreter ()
ensureBothNumber _ (Number _) (Number _) = return ()
ensureBothNumber op _ _ = throwError $ ArgumentError ("Both arguments to " ++ show op ++ " must be Numbers")

stringify :: RuntimeValue -> String
stringify (String str) = str
stringify Null = "nil"
stringify (Boolean b) = show b
stringify (NativeFunction _ name _) = "native function: " ++ name
stringify (LoxFunction _ name _ _ _ _) = "function: " ++ name
stringify (LoxClass name _superclass methods) = "class: " ++ name ++ " " ++ show (M.keys methods)
stringify (LoxInstance klass _fields) = "instance: " ++ stringify klass
stringify (Number num) = fixedNum
  where fixedNum = if take 2 (reverse shownNum) == "0." then init . init $ shownNum else shownNum
        shownNum = show num
