module GoogleSheets (
  GoogleSheetsEff,
  getValues,
  batchUpdate
) where

import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (Json)

import Auth (Oauth2Client)

foreign import data GoogleSheetsEff :: !

foreign import getValues :: forall e.
  { auth :: Oauth2Client, spreadsheetId :: String, range :: String } ->
  Aff (getValues :: GoogleSheetsEff | e) Json

foreign import batchUpdate :: forall e.
  { auth :: Oauth2Client, spreadsheetId :: String } ->
  Aff (getValues :: GoogleSheetsEff | e) String
