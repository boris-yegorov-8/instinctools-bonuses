module Client (getClient) where

import Control.Monad.Aff (Aff, attempt)
import Control.Semigroupoid ((<<<))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.Either (either)
import Data.Function (($))
import Control.Monad.Except (runExcept)
import Data.Foreign.Class (readJSON)
import Data.Foreign (F)
import Control.Bind ((>=>))
import Control.Applicative (pure)
import Node.FS (FS)
import Control.Monad.Eff.Exception (EXCEPTION)

import Credentials.ClientSecret (ClientSecret(..))
import Auth (Oauth2Client, createClient)
import Util (throwError, throwWrappedError)

getClient :: forall e.
  String -> Aff (fs :: FS, err :: EXCEPTION | e) Oauth2Client
getClient = attempt <<< readTextFile UTF8 >=> either
  (throwWrappedError "Loading client secret file failed: ")
  (\content -> either
    (throwError "Parsing client secret JSON failed: ")
    (\(ClientSecret c) -> pure $ createClient c)
    (runExcept $ readJSON content :: F ClientSecret))
