module Google.Sheets.Request (
  Request(..),
  InsertDimensionRecord,
  DeleteDimensionRecord,
  UpdateCellsRecord,
  CopyPasteRecord,
  Cell(..),
  CellNumberRecord,
  CellStringRecord
) where

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

type UpdateCellsRecord =
  {
    updateCells ::
      {
        start ::
          {
            sheetId :: Number,
            rowIndex :: Number,
            columnIndex :: Number
          },
        rows :: Array { values :: Array Cell },
        fields :: String
      }
  }

type CopyPasteRecord =
  {
    copyPaste ::
      {
        source ::
          {
            sheetId :: Number,
            startRowIndex :: Number,
            endRowIndex :: Number,
            startColumnIndex :: Number,
            endColumnIndex :: Number
          },
        destination ::
          {
            sheetId :: Number,
            startRowIndex :: Number,
            endRowIndex :: Number,
            startColumnIndex :: Number,
            endColumnIndex :: Number
          },
        pasteType :: String
      }
  }

type CellNumberRecord = { userEnteredValue :: { numberValue :: Number } }
type CellStringRecord = { userEnteredValue :: { stringValue :: String } }

data Cell = CellNumber CellNumberRecord | CellString CellStringRecord

data Request =
  InsertDimension InsertDimensionRecord |
  DeleteDimension DeleteDimensionRecord |
  UpdateCells UpdateCellsRecord |
  CopyPaste CopyPasteRecord
