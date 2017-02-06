module Sheet (updateSheet) where

import GoogleSheets (getValues)
import Constants (sheetId)

updateSheet client message = getValues options
  where
    options =
      {
        auth: client,
        spreadsheetId: sheetId,
        range: "A2:I155"
      }
