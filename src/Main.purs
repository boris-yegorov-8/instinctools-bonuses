module Main where

import Data.Show (class Show, show)
import Data.Unit (Unit)
import Data.Function (($), (#))
import Data.Semigroup ((<>))
import Control.Applicative (pure)
import Control.Bind (bind, (>=>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)

import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)

import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import Data.Foreign.Index (prop)

data Installed = Installed String String

instance showFoo :: Show Installed where
  show (Installed bar baz) = "(Installed " <> show bar <> " " <> show baz <> ")"

instance fooIsForeign :: IsForeign Installed where
  read value = do
    s <- value # (prop "foo" >=> readProp "bar")
    n <- value # (prop "foo" >=> readProp "baz")
    pure $ Installed s n

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  -- clientSecret <- readTextFile UTF8 "./credentials/client_secret.json"
  -- logShow clientSecret
  logShow $ runExcept $ readJSON """{ "foo": { "bar": "bar", "baz": "1" } }""" :: F Installed
