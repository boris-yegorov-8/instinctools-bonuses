module GoogleSheets (
  GoogleSheetsEff,
  Request(..),
  InsertDimensionRecord,
  DeleteDimensionRecord,
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

type InsertDimensionRecord =
  {
    insertDimension ::
      {
        range ::
          {
            sheetId :: Number,
            dimension :: String,
            startIndex :: Number,
            endIndex :: Number
          },
        inheritFromBefore :: Boolean
      }
  }

type DeleteDimensionRecord =
  {
    deleteDimension ::
      {
        range ::
          {
            sheetId :: Number,
            dimension :: String,
            startIndex :: Number,
            endIndex :: Number
          }
      }
  }

data Request =
  InsertDimension InsertDimensionRecord | DeleteDimension DeleteDimensionRecord

foreign import batchUpdate :: forall e.
  {
    auth :: Oauth2Client,
    spreadsheetId :: String,
    resource :: { requests :: Array Request }
  } ->
  Aff (getValues :: GoogleSheetsEff | e) String
