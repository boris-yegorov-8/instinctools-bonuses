module Gmail (GmailEff, users) where

import Data.Unit (Unit)
import Control.Monad.Eff (Eff)

foreign import data GmailEff :: !
foreign import users :: forall eff.
                        (Number -> Eff (users :: GmailEff | eff) Unit)
                     -> Eff (users :: GmailEff | eff) Unit
