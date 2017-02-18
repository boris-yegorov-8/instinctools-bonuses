module Main where

import Control.Bind ((>>=))
import Control.Monad.Aff (Canceler, runAff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Data.Function (($))
import Control.Monad.Eff (Eff)
import Node.FS (FS)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.ReadLine (READLINE)
import Node.Buffer (BUFFER)

import Credentials.Token (Token(..))
import Client (getClient)
import Token (getToken)
import Email (getMessage)
import Auth (AuthEff, setToken)
import Constants as Constants
import Sheet (updateSheet)
import Google.Gmail (GmailEff)
import Google.Sheets (GoogleSheetsEff)

main :: forall e.
  Eff
    ( console :: CONSOLE
    , fs :: FS
    , err :: EXCEPTION
    , readline :: READLINE
    , getToken :: AuthEff
    , getMessages :: GmailEff
    , buffer :: BUFFER
    , getValues :: GoogleSheetsEff
    | e
    )
    (Canceler
       ( console :: CONSOLE
       , fs :: FS
       , err :: EXCEPTION
       , readline :: READLINE
       , getToken :: AuthEff
       , getMessages :: GmailEff
       , buffer :: BUFFER
       , getValues :: GoogleSheetsEff
       | e
       )
    )
main = runAff
  logShow
  (\_ -> log "Successfully finished")
  (
    (getClient Constants.clientSecretPath) >>=
    (\client ->
      (getToken client) >>=
      (\(Token t) -> getMessage $ setToken t) >>=
      (updateSheet client)
    )
  )
