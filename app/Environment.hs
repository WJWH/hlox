module Environment where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Time.Clock ( nominalDiffTimeToSeconds )
import Data.Time.Clock.POSIX

import Types

mkRootEnv :: Env
mkRootEnv = Env Nothing rootBindings
  where rootBindings = M.insert "clock" clockFunc M.empty
        clockFunc = NativeFunction 0 $ \_ -> Number . realToFrac . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime

mkChildEnv :: Env -> Env
mkChildEnv parent = Env (Just parent) M.empty

-- setting and getting variables
defineVar :: String -> RuntimeValue -> Interpreter ()
defineVar name value = do
  (Env parent bindings) <- gets env
  modify' $ \s -> s { env = Env parent (M.insert name value bindings) }

getVar :: String -> Interpreter RuntimeValue
getVar name = do
  currentEnv <- gets env
  case findVar name currentEnv of
    Nothing -> throwError . RuntimeError $ "Undefined variable: '" ++ name ++ "'."
    Just val -> return val

assignVar :: String -> RuntimeValue -> Interpreter RuntimeValue
assignVar name val = do
  currentEnv <- gets env
  case setVar name val currentEnv of
    Nothing -> throwError . RuntimeError $ "Undefined variable: '" ++ name ++ "'."
    Just updatedEnv -> (modify' $ \s -> s { env = updatedEnv }) >> return val

-- Unlike loxomotive, our env don't have any IORef use. The cost is that the logic is more
-- complex and not very much like the book though.
findVar :: String -> Env -> Maybe RuntimeValue
findVar name (Env parent bindings) = case M.lookup name bindings of
  Just val -> Just val -- if the value is available in the current scope, just use that
  Nothing -> case parent of
    Nothing -> Nothing -- we're in the root scope, so no parent to recurse into
    Just p -> findVar name p -- maybe the value is available in the parent scope or above?

-- return Nothing if the variable was not found in the env or any of its parents, returns the
-- modified env wrapped in Just if it was found and updated.
setVar :: String -> RuntimeValue -> Env -> Maybe Env
setVar name val (Env parent bindings) = case M.lookup name bindings of
  Just _ -> Just $ Env parent $ M.insert name val bindings
  Nothing -> case parent of
    Nothing -> Nothing -- no parent available, so the var to update couldn't be found
    Just p -> case setVar name val p of
      Nothing -> Nothing -- , var could'nt be found upstream, pass on the Nothing
      Just updatedParent -> Just $ Env (Just updatedParent) bindings
