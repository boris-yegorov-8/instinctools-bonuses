module Main where

import Control.Bind ((>>=))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (logShow, log)
import Data.Function (($))

import Credentials.Token (Token(..))
import Client (getClient)
import Token (getToken)
import Email (getMessage)
import Auth as Auth
import Constants as Constants
import Sheet (updateSheet)

main = runAff
  logShow
  (\_ -> log "Successfully finished")
  (
    (getClient Constants.clientSecretPath) >>=
    (\client ->
      (getToken client) >>=
      (\(Token t) -> getMessage $ Auth.setToken t) >>=
      (updateSheet client)
    )
  )
