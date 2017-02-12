module Google.Sheets (
  GoogleSheetsEff,
  getValues,
  batchUpdate
) where

import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (Json)
import Data.Unit (Unit)

import Auth (Oauth2Client)

foreign import data GoogleSheetsEff :: !

foreign import getValues :: forall e.
  { auth :: Oauth2Client, spreadsheetId :: String, range :: String } ->
  Aff (getValues :: GoogleSheetsEff | e) Json

foreign import batchUpdate :: forall e.
  {
    auth :: Oauth2Client,
    spreadsheetId :: String,
    resource :: String
  } ->
  Aff (getValues :: GoogleSheetsEff | e) Unit
