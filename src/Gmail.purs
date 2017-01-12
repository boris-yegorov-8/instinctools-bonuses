module Gmail (
  GmailEff,
  GmailOptions,
  GetMessagesEff,
  getMessages
) where

import Data.Unit (Unit)
import Control.Monad.Eff (Eff)

import Auth (Oauth2Client)

type GmailOptions = {
  auth :: Oauth2Client,
  userId :: String,
  q :: String
}

type GetMessagesEff eff = Eff (getMessages :: GmailEff | eff) Unit

foreign import data GmailEff :: !
foreign import getMessages :: forall eff.
                        GmailOptions
                     -> (String -> Array {id :: String} -> GetMessagesEff eff)
                     -> GetMessagesEff eff
