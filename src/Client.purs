module Client (getClient) where

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (logShow)
import Control.Semigroupoid ((<<<))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.Either (either)
import Data.Function (($))
import Control.Monad.Except (runExcept)
import Data.Foreign.Class (readJSON)
import Data.Foreign (F)
import Control.Bind ((>>=))
import Control.Monad.Eff.Class (liftEff)

import Util (logError)
import Credentials.ClientSecret (ClientSecret(..))
import Auth as Auth

getClient path = (readTextFile UTF8 path) >>= (\_ -> liftEff $ logShow 42)
  -- let
  --   onError = logError "Loading client secret file failed: "
  --   onSuccess = \clientSecretContent -> either
  --     (logError "Wrong credentials: ")
  --     logShow
  --     -- (\(ClientSecret c) -> Auth.createClient c)
  --     (runExcept $ readJSON clientSecretContent :: F ClientSecret)
  -- in
  --   runAff onError onSuccess <<<
