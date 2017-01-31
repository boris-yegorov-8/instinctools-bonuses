module Main where

import Control.Bind ((>>=))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (logShow)
import Credentials.Token (Token(..))
import Data.Function (($))

import Client (getClient)
import Token (getToken)
import Email (getMessage)
import Auth as Auth
import Constants as Constants

main = runAff
  logShow
  (\_ -> logShow 73)
  -- (\_ -> log "Successfully finished")
  (
    (getClient Constants.clientSecretPath) >>=
    getToken >>=
    (\(Token t) -> getMessage $ Auth.setToken t)
  )
