module Util (logError) where

import Data.Show (class Show, show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Unit (Unit)
import Control.Semigroupoid ((<<<))
import Data.Semigroup ((<>))

logError :: forall e err. (Show err) =>
  String -> err -> Eff (console :: CONSOLE | e) Unit
logError prefix = log <<< (<>) prefix <<< show
