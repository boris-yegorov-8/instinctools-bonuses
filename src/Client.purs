module Client (getClient) where

import Control.Monad.Aff (Aff, attempt)
import Control.Semigroupoid ((<<<))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Data.Either (either)
import Data.Function (($))
import Control.Monad.Except (runExcept)
import Control.Bind ((=<<), (>=>))
import Control.Applicative (pure)
import Node.FS (FS)
import Control.Monad.Eff.Exception (EXCEPTION)

import Credentials.ClientSecret (ClientSecret(..), readClientSecret)
import Auth (Oauth2Client, createClient)
import Util (throwError, throwWrappedError)
import JsonParser (toForeign)

getClient :: forall e.
  String -> Aff (exception :: EXCEPTION, fs :: FS | e) Oauth2Client
getClient = attempt <<< readTextFile UTF8 >=> either
  (throwWrappedError "Loading client secret file failed: ")
  (\content -> either
    (throwError "Parsing client secret JSON failed: ")
    (\(ClientSecret c) -> pure $ createClient c)
    (runExcept $ readClientSecret =<< toForeign content))
