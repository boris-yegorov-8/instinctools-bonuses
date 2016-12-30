module Gmail (GmailEff, users) where

import Data.Unit (Unit)
import Control.Monad.Eff (Eff)

import Auth (Oauth2Client)

foreign import data GmailEff :: !
foreign import users :: forall eff.
                        Oauth2Client
                     -> (Number -> Eff (users :: GmailEff | eff) Unit)
                     -> Eff (users :: GmailEff | eff) Unit
