module Gmail (GmailOptions, GmailEff, getMessages) where

import Data.Unit (Unit)
import Control.Monad.Eff (Eff)

import Auth (Oauth2Client)

type GmailOptions = {
  auth :: Oauth2Client,
  userId :: String,
  q :: String
}

type GetMessagesEff = Eff (getMessages :: GmailEff | eff) Unit

foreign import data GmailEff :: !
foreign import getMessages :: forall eff.
                        GmailOptions
                     -> (Number -> Number -> GetMessagesEff)
                     -> GetMessagesEff
