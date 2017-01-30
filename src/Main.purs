module Main where

import Auth as Auth
import Constants as Constants
import Control.Bind (class Bind, (>>=), (>=>))
import Control.Monad.Aff (Aff, attempt, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message, error)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Credentials.ClientSecret (ClientSecret(..))
import Credentials.Token (Token(..))
import Data.Argonaut.Core (Json)
import Data.Either (Either, either)
import Data.Foreign (F, ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmptyList)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import Gmail (GmailEff, getMessages)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.ReadLine as ReadLine
import Control.Applicative (pure)

import Client (getClient)
import Token (getToken)
import Email (getMessage)

main = runAff
  logShow
  logShow
  -- (\_ -> log "Successfully finished")
  (
    (getClient Constants.clientSecretPath) >>=
    getToken >>=
    (\(Token t) -> getMessage $ Auth.setToken t)
  )
