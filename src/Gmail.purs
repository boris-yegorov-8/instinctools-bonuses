module Gmail (
  GmailEff,
  GmailOptions,
  getMessages
) where

import Control.Monad.Aff (Aff)

import Auth (Oauth2Client)

type GmailOptions = {
  auth :: Oauth2Client,
  userId :: String,
  q :: String
}

foreign import data GmailEff :: !
foreign import getMessages :: forall eff.
  GmailOptions -> Aff (getMessages :: GmailEff | eff) (Array {id :: String})
