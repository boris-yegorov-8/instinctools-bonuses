module GoogleSheets (
  GoogleSheetsEff,
  getValues
) where

import Control.Monad.Aff (Aff)

import Auth (Oauth2Client)

foreign import data GoogleSheetsEff :: !

foreign import getValues :: forall e.
  { auth :: Oauth2Client, spreadsheetId :: String, range :: String } ->
  Aff (getValues :: GoogleSheetsEff | e) String
