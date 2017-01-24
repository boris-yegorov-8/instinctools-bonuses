module Client (getClient) where

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Console (logShow)
import Control.Semigroupoid ((<<<))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.Either (either)
import Data.Function (($))
import Control.Monad.Except (runExcept)
import Data.Foreign.Class (readJSON)
import Data.Foreign (F)
import Control.Bind ((>>=), (>=>))
import Control.Monad.Eff.Class (liftEff)
import Data.Semigroup ((<>))
import Data.Show (show)
import Control.Monad.Eff.Exception (Error, error, message, throwException)

import Credentials.ClientSecret (ClientSecret(..))
import Auth as Auth

getClient = attempt <<< readTextFile UTF8 >=> liftEff <<< either
  (throwException <<< wrapError)
  logShow
  where
    wrapError =
      error <<< (<>) "Loading client secret file failed: " <<< message
  --   onSuccess = \clientSecretContent -> either
  --     (logError "Wrong credentials: ")
  --     logShow
  --     -- (\(ClientSecret c) -> Auth.createClient c)
  --     (runExcept $ readJSON clientSecretContent :: F ClientSecret)
  -- in
  --   runAff onError onSuccess <<<
