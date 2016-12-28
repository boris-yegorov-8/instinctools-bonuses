module Auth (Options, Oauth2Client, createClient) where

import Credentials.Token (TokenObject)

foreign import data Oauth2Client :: *

type Options = {
  client_id :: String,
  client_secret :: String,
  redirect_uri :: String,
  token :: TokenObject
}

foreign import createClient :: Options -> Oauth2Client
