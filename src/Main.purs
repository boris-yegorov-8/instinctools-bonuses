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

data Credentials = Credentials String String (Array String)

instance showFoo :: Show Credentials where
  show (Credentials clientId clientSecret redirectUris) =
    "(Credentials " <> show clientId <> " " <> show clientSecret <> " " <> show redirectUris <> ")"

instance fooIsForeign :: IsForeign Credentials where
  read value = do
    clientId <- value # (prop "installed" >=> readProp "client_id")
    clientSecret <- value # (prop "installed" >=> readProp "client_secret")
    redirectUris <- value # (prop "installed" >=> readProp "redirect_uris")
    pure $ Credentials clientId clientSecret redirectUris

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  clientSecret <- readTextFile UTF8 "./credentials/client_secret.json"
  logShow $ runExcept $ readJSON clientSecret :: F Credentials
