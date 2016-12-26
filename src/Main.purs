module Main where

import Data.Unit (Unit)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)

import Credentials.ClientSecret (ClientSecret)
import Auth (createClient)
import Gmail (GmailEff, users)



main :: forall e. Eff (users :: GmailEff, console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  clientSecret <- readTextFile UTF8 "./credentials/client_secret.json"
  logShow $ runExcept $ readJSON clientSecret :: F ClientSecret
