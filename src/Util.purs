module Util (throwError, throwWrappedError) where

import Control.Semigroupoid ((<<<))
import Data.Semigroup ((<>))
import Control.Monad.Eff.Exception (
  Error,
  EXCEPTION,
  error,
  message,
  throwException
)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)

throwError :: forall e a. String -> Aff (err :: EXCEPTION | e) a
throwError = liftEff <<< throwException <<< error

throwWrappedError :: forall e a.
  String -> Error -> Aff (err :: EXCEPTION | e) a
throwWrappedError prefix = throwError <<< (<>) prefix <<< message
