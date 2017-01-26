module Util (throwError, throwWrappedError, then', (>>)) where

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
import Control.Bind (class Bind, (>>=))

throwError :: forall e a. String -> Aff (err :: EXCEPTION | e) a
throwError = liftEff <<< throwException <<< error

throwWrappedError :: forall e a.
  String -> Error -> Aff (err :: EXCEPTION | e) a
throwWrappedError prefix = throwError <<< (<>) prefix <<< message

then' :: forall m a b. (Bind m) => m a -> m b -> m b
then' ma mb = ma >>= (\_ -> mb)

infixl 1 then' as >>
