module Main where

import Data.Function (($))
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
import Credentials.Token (Token)
import Auth (createClient)
import Gmail (GmailEff, users)

readTextFileUtf8 :: forall t.
  String -> Eff (fs :: FS, err :: EXCEPTION | t) String
readTextFileUtf8 = readTextFile UTF8

main :: forall e. Eff (users :: GmailEff, console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  clientSecretContent <- readTextFileUtf8 "./credentials/client_secret.json"
  tokenContent <- readTextFileUtf8 "./credentials/credentials.json"
  clientSecret <- runExcept $ readJSON clientSecretContent :: F ClientSecret
  logShow clientSecret
  -- logShow $ runExcept $ readJSON token :: F Token
