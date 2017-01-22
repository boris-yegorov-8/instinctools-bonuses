module Auth (
  AuthEff,
  GetTokenEff,
  Options,
  Oauth2Client,
  createClient,
  setToken,
  generateAuthUrl,
  getToken
) where

import Data.Unit (Unit)
import Control.Monad.Eff (Eff)
import Credentials.Token (TokenObject)

foreign import data Oauth2Client :: *

type Options = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

type GetTokenEff eff = Eff (getToken :: AuthEff | eff) Unit

foreign import data AuthEff :: !

foreign import createClient :: Options -> Oauth2Client

foreign import setToken :: TokenObject -> Oauth2Client

foreign import generateAuthUrl :: Oauth2Client
                               -> { access_type :: String, scope :: String }
                               -> String

foreign import getToken :: forall eff.
                        Oauth2Client
                     -> String
                     -> (String -> String -> GetTokenEff eff)
                     -> GetTokenEff eff
