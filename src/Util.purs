module Util (throwWrappedError) where

import Control.Monad.Eff (Eff)
import Control.Semigroupoid ((<<<))
import Data.Semigroup ((<>))
import Control.Monad.Eff.Exception (
  Error,
  EXCEPTION,
  error,
  message,
  throwException
)

throwWrappedError :: forall e a.
  String -> Error -> Eff (err :: EXCEPTION | e) a
throwWrappedError prefix = throwException <<< error <<< (<>) prefix <<< message
