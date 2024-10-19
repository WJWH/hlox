module Environment where

import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock ( nominalDiffTimeToSeconds )
import Data.Time.Clock.POSIX

import Types

-- making new Environments
mkRootEnv :: IO Env
mkRootEnv = do
  bindingsRef <- liftIO $ newIORef rootBindings
  return $ Env Nothing bindingsRef
  where rootBindings = M.insert "clock" clockFunc M.empty
        clockFunc = NativeFunction 0 "clock" $ \_ -> Number . realToFrac . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime

mkChildEnv :: Env -> Interpreter Env
mkChildEnv parent = do
  bindingsRef <- liftIO $ newIORef M.empty
  return $ Env (Just parent) bindingsRef

-- setting and getting variables in the Interpreter monad using the Env from its state
defineVar :: String -> RuntimeValue -> Interpreter ()
defineVar name value = do
  (Env _parent bindingsRef) <- gets env
  liftIO $ modifyIORef bindingsRef $ M.insert name value

getVar :: String -> Interpreter RuntimeValue
getVar name = do
  currentEnv <- gets env
  foundVar <- findVar name currentEnv
  case foundVar of
    Nothing -> throwError . RuntimeError $ "Undefined variable read: '" ++ name ++ "'."
    Just val -> return val

getVarAt :: Int -> String -> Interpreter RuntimeValue
getVarAt depth name = do
  currentEnv <- gets env
  targetEnv <- ancestor depth currentEnv
  targetBindings <- liftIO . readIORef $ bindings targetEnv
  case M.lookup name targetBindings of
    Nothing -> throwError . RuntimeError $ "Should never happen, tried to access variable that doesn't exist (or locals is wrong)"
    Just var -> return var


assignVar :: String -> RuntimeValue -> Interpreter RuntimeValue
assignVar name val = do
  currentEnv <- gets env
  settedVar <- setVar name val currentEnv
  case settedVar of
    Nothing -> throwError . RuntimeError $ "Undefined variable write: '" ++ name ++ "'."
    Just updatedEnv -> (modify' $ \s -> s { env = updatedEnv }) >> return val

setVarAt :: Int -> String -> RuntimeValue -> Interpreter RuntimeValue
setVarAt depth name val = do
  currentEnv <- gets env
  targetEnv <- ancestor depth currentEnv
  liftIO $ modifyIORef (bindings targetEnv) $ M.insert name val
  return val

-- getting and setting variables in an Environment given in the arguments
findVar :: String -> Env -> Interpreter (Maybe RuntimeValue)
findVar name (Env parent bindingsRef) = do
  bindings <- liftIO $ readIORef bindingsRef
  case M.lookup name bindings of
    Just val -> return $ Just val -- if the value is available in the current scope, just use that
    Nothing -> case parent of
      Nothing -> return Nothing -- we're in the root scope, so no parent to recurse into
      Just p -> findVar name p -- maybe the value is available in the parent scope or above?

setVar :: String -> RuntimeValue -> Env -> Interpreter (Maybe Env)
setVar name val (Env parent bindingsRef) = do
  bindings <- liftIO $ readIORef bindingsRef
  case M.lookup name bindings of
    Just _ -> do
      liftIO $ modifyIORef bindingsRef $ M.insert name val
      return $ Just $ Env parent bindingsRef
    Nothing -> case parent of
      Nothing -> return Nothing -- no parent available, so the var to update couldn't be found
      Just p -> setVar name val p -- maybe the parent has the var, recurse upward

-- used for defining 'this' in the scope of class methods. Since it mutates the env in-place, no return
-- value is needed
defineVarRaw :: String -> RuntimeValue -> Env -> Interpreter ()
defineVarRaw name val (Env _parent bindingsRef) = liftIO $ modifyIORef bindingsRef $ M.insert name val

-- utility functions
ancestor :: Int -> Env -> Interpreter Env
ancestor 0 env = return $ env
ancestor depth env = case parent env of
  Nothing -> throwError . RuntimeError $ "Should never happen, tried to access ancestor that doesn't exist"
  Just directAncestor -> ancestor (depth - 1) directAncestor
