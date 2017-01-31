module Gmail (
  GmailEff,
  getMessages,
  getMessage
) where

import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (Json)

import Auth (Oauth2Client)

foreign import data GmailEff :: !

foreign import getMessages :: forall eff.
  { auth :: Oauth2Client, userId :: String, q :: String } ->
  Aff (getMessages :: GmailEff | eff) (Array {id :: String})

foreign import getMessage :: forall eff.
  { auth :: Oauth2Client, userId :: String, id :: String } ->
  Aff (getMessages :: GmailEff | eff) Json
