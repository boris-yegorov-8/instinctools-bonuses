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

data Foo = Foo Bar Baz

data Bar = Bar String

data Baz = Baz Number

instance showFoo :: Show Foo where
  show (Foo bar baz) = "(Foo " <> show bar <> " " <> show baz <> ")"

instance showBar :: Show Bar where
  show (Bar s) = "(Bar " <> show s <> ")"

instance showBaz :: Show Baz where
  show (Baz n) = "(Baz " <> show n <> ")"

instance fooIsForeign :: IsForeign Foo where
  read value = do
    s <- value # (prop "foo" >=> readProp "bar")
    n <- value # (prop "foo" >=> readProp "baz")
    pure $ Foo (Bar s) (Baz n)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  -- log =<< readTextFile UTF8 "./credentials/client_secret.json"
  logShow $ runExcept $ readJSON """{ "foo": { "bar": "bar", "baz": 1 } }""" :: F Foo
