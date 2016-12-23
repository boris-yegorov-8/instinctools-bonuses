module Main where

import Data.Show (class Show)
import Data.Unit (Unit)
import Data.Function (($), (#))
import Control.Semigroupoid ((<<<))
import Data.Array (head, fold)
import Data.Maybe (fromMaybe)
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

import Auth (createClient)
import Gmail (users)

data Credentials = Credentials String String String

instance showFoo :: Show Credentials where
  show (Credentials clientId clientSecret redirectUri) = fold [
      "(Credentials ",
      clientId,
      " ",
      clientSecret,
      " ",
      redirectUri,
      ")"
    ]

firstRedirectUri :: Array String -> String
firstRedirectUri = (fromMaybe "http://localhost") <<< head

instance fooIsForeign :: IsForeign Credentials where
  read value = do
    clientId <- value # (prop "installed" >=> readProp "client_id")
    clientSecret <- value # (prop "installed" >=> readProp "client_secret")
    redirectUris <- value # (prop "installed" >=> readProp "redirect_uris")
    pure $ Credentials clientId clientSecret $ firstRedirectUri redirectUris

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  logShow $ users $ createClient "42"
  -- clientSecret <- readTextFile UTF8 "./credentials/client_secret.json"
  -- logShow $ runExcept $ readJSON clientSecret :: F Credentials
