module Auth (
  Options,
  Oauth2Client,
  createClient,
  setToken,
  generateAuthUrl
) where

import Credentials.Token (TokenObject)

foreign import data Oauth2Client :: *

type Options = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

foreign import createClient :: Options -> Oauth2Client
foreign import setToken :: TokenObject -> Oauth2Client
foreign import generateAuthUrl :: Oauth2Client
                               -> { access_type :: String, scope :: String }
                               -> String
