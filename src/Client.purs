module Client (getClient) where

import Control.Monad.Aff (Aff(..), attempt)
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
import Control.Monad.Eff.Exception (throwException, error)
import Control.Applicative (pure)

import Credentials.ClientSecret (ClientSecret(..))
import Auth as Auth
import Util (throwWrappedError)

getClient = attempt <<< readTextFile UTF8 >=> either
  (liftEff <<< throwWrappedError "Loading client secret file failed: ")
  (\content -> either
    (\_ -> liftEff $ throwException $ error "Parsing client secret JSON failed: ")
    (\(ClientSecret c) -> pure $ Auth.createClient c)
    (runExcept $ readJSON content :: F ClientSecret))
