module Credentials.ClientSecret (ClientSecret(..), ClientSecretObject, readClientSecret) where

import Data.Array (head)
import Data.Maybe (fromMaybe)
import Control.Bind (bind, (>>=))
import Data.Function (($))
import Data.Show (class Show)
import Data.Foreign.Index ((!))
import Control.Applicative (pure)
import Data.Foreign (F, Foreign, readString, readArray)
import Data.Traversable (traverse)

type ClientSecretObject = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

data ClientSecret = ClientSecret ClientSecretObject

readClientSecret :: Foreign -> F ClientSecret
readClientSecret value = do
  clientId <- value ! "installed" ! "client_id" >>= readString
  clientSecret <- value ! "installed" ! "client_secret" >>= readString
  redirectUris <- value ! "installed" ! "redirect_uris" >>= readArray >>= traverse readString
  pure $ ClientSecret {
    clientId,
    clientSecret,
    redirectUri: (fromMaybe "http://localhost") $ head redirectUris
  }
