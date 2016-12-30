module Auth (Options, Oauth2Client, createClient) where

import Credentials.Token (TokenObject)

foreign import data Oauth2Client :: *

type Options = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String,
  token :: TokenObject
}

foreign import createClient :: Options -> Oauth2Client
