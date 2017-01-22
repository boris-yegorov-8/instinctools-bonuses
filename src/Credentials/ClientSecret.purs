module Credentials.ClientSecret (ClientSecret(..), ClientSecretObject) where

import Data.Array (head, fold)
import Data.Maybe (fromMaybe)
import Control.Bind (bind, (>=>))
import Data.Function (($), (#))
import Data.Show (class Show)
import Data.Foreign.Index (prop)
import Data.Foreign.Class (class IsForeign, readProp)
import Control.Applicative (pure)

type ClientSecretObject = {
  clientId :: String,
  clientSecret :: String,
  redirectUri :: String
}

data ClientSecret = ClientSecret ClientSecretObject

instance showFoo :: Show ClientSecret where
  show (ClientSecret o) = fold [
      "(Credentials { clientId: ",
      o.clientId,
      ", clientSecret: ",
      o.clientSecret,
      ", redirectUri: ",
      o.redirectUri,
      " })"
    ]

instance fooIsForeign :: IsForeign ClientSecret where
  read value = do
    clientId <- value # (prop "installed" >=> readProp "client_id")
    clientSecret <- value # (prop "installed" >=> readProp "client_secret")
    redirectUris <- value # (prop "installed" >=> readProp "redirect_uris")
    pure $ ClientSecret {
      clientId,
      clientSecret,
      redirectUri: (fromMaybe "http://localhost") $ head redirectUris
    }
