module Credentials.ClientSecret (ClientSecret(..)) where

import Data.Array (head, fold)
import Control.Semigroupoid ((<<<))
import Data.Maybe (fromMaybe)
import Control.Bind (bind, (>=>))
import Data.Function (($), (#))
import Data.Show (class Show)
import Data.Foreign.Index (prop)
import Data.Foreign.Class (class IsForeign, readProp)
import Control.Applicative (pure)

data ClientSecret = ClientSecret String String String

instance showFoo :: Show ClientSecret where
  show (ClientSecret clientId clientSecret redirectUri) = fold [
      "(Credentials ",
      clientId,
      " ",
      clientSecret,
      " ",
      redirectUri,
      ")"
    ]

firstRedirectUri :: Array String -> String
firstRedirectUri = (fromMaybe "http://localhost") <<< head

instance fooIsForeign :: IsForeign ClientSecret where
  read value = do
    clientId <- value # (prop "installed" >=> readProp "client_id")
    clientSecret <- value # (prop "installed" >=> readProp "client_secret")
    redirectUris <- value # (prop "installed" >=> readProp "redirect_uris")
    pure $ ClientSecret clientId clientSecret $ firstRedirectUri redirectUris
