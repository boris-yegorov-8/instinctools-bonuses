module Auth (
  AuthEff,
  Options,
  Oauth2Client,
  createClient,
  setToken,
  generateAuthUrl,
  getToken
) where

import Data.Argonaut.Core (Json)
import Credentials.Token (TokenObject)
import Control.Monad.Aff (Aff)

foreign import data Oauth2Client :: *

type Options = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

foreign import data AuthEff :: !

foreign import createClient :: Options -> Oauth2Client

foreign import setToken :: TokenObject -> Oauth2Client

foreign import generateAuthUrl ::
  Oauth2Client ->
  { access_type :: String, scope :: Array String } ->
  String

foreign import getToken :: forall eff.
                        Oauth2Client
                     -> String
                     -> Aff (getToken :: AuthEff | eff) Json
