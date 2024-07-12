module Resolver where

import Control.Monad.State
import qualified Data.Map as M

import Types


resolveStatement :: Statement -> Resolver ()

resolveExpression :: Expression -> Resolver ()

-- Begins variable definition process
declare :: String -> Resolver ()

-- finishes variable definition process
define :: String -> Resolver ()


beginScope :: Resolver ()


endScope :: Resolver ()
