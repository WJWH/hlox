module Environment where

import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock ( nominalDiffTimeToSeconds )
import Data.Time.Clock.POSIX

import Types

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

-- setting and getting variables
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

assignVar :: String -> RuntimeValue -> Interpreter RuntimeValue
assignVar name val = do
  currentEnv <- gets env
  settedVar <- setVar name val currentEnv
  case settedVar of
    Nothing -> throwError . RuntimeError $ "Undefined variable write: '" ++ name ++ "'."
    Just updatedEnv -> (modify' $ \s -> s { env = updatedEnv }) >> return val

-- Unlike loxomotive, our env don't have any IORef use. The cost is that the logic is more
-- complex and not very much like the book though.
findVar :: String -> Env -> Interpreter (Maybe RuntimeValue)
findVar name (Env parent bindingsRef) = do
  bindings <- liftIO $ readIORef bindingsRef
  case M.lookup name bindings of
    Just val -> return $ Just val -- if the value is available in the current scope, just use that
    Nothing -> case parent of
      Nothing -> return Nothing -- we're in the root scope, so no parent to recurse into
      Just p -> findVar name p -- maybe the value is available in the parent scope or above?

-- return Nothing if the variable was not found in the env or any of its parents, returns the
-- modified env wrapped in Just if it was found and updated.
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
