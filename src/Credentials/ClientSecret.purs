module Credentials.ClientSecret (ClientSecret(..), ClientSecretObject) where

import Data.Array (head)
import Data.Maybe (fromMaybe)
import Control.Bind (bind, (>>=))
import Data.Function (($))
import Data.Show (class Show)
import Data.Foreign.Index ((!))
import Data.Foreign.Class (class Decode)
import Control.Applicative (pure)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign (readString, readArray)
import Data.Traversable (traverse)

type ClientSecretObject = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

data ClientSecret = ClientSecret ClientSecretObject

derive instance genericClientSecret :: Generic ClientSecret _

instance showClientSecret :: Show ClientSecret where
  show = genericShow

instance decodeClientSecret :: Decode ClientSecret where
  decode value = do
    clientId <- value ! "installed" ! "client_id" >>= readString
    clientSecret <- value ! "installed" ! "client_secret" >>= readString
    redirectUris <- value ! "installed" ! "redirect_uris" >>= readArray >>= (traverse readString)
    pure $ ClientSecret {
      clientId,
      clientSecret,
      redirectUri: (fromMaybe "http://localhost") $ head redirectUris
    }
