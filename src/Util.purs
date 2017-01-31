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
import Control.Monad.Aff (Aff)
import Data.Function (const)

throwError :: forall a b c e.
  (MonadEff
     ( err :: EXCEPTION
     | e
     )
     b
  ) => String -> a -> b c
throwError = const <<< liftEff <<< throwException <<< error

throwError' :: forall e a. String -> Aff (err :: EXCEPTION | e) a
throwError' = liftEff <<< throwException <<< error

throwWrappedError :: forall e a.
  String -> Error -> Aff (err :: EXCEPTION | e) a
throwWrappedError prefix = throwError' <<< (<>) prefix <<< message
