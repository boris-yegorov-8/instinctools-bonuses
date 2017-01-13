module Auth (Options, Oauth2Client, createClient, setToken) where

import Credentials.Token (TokenObject)

foreign import data Oauth2Client :: *

type Options = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

foreign import createClient :: Options -> Oauth2Client
foreign import setToken :: TokenObject -> Oauth2Client
