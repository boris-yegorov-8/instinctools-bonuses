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
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Function (const)

throwError :: forall a b c e.
  (MonadEff (exception :: EXCEPTION | e) b) => String -> a -> b c
throwError = const <<< throwError'

throwError' :: forall e a b.
  (MonadEff (exception :: EXCEPTION | e) a) => String -> a b
throwError' = liftEff <<< throwException <<< error

throwWrappedError :: forall b a e.
  (MonadEff (exception :: EXCEPTION | e) a) => String -> Error -> a b
throwWrappedError prefix = throwError' <<< (<>) prefix <<< message
