module Sheet (Values(..), updateSheet) where

import Constants (sheetId)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=), (=<<), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Array (
  length,
  concat,
  drop,
  filter,
  head,
  sortBy,
  union,
  (!!)
)
import Data.Either (either)
import Data.Eq ((==))
import Data.Foreign (F, Foreign, readArray, readString)
import Data.Foreign.Index ((!))
import Data.Foreign.Class (class Decode)
import Data.Function (($))
import Data.Functor ((<#>), (<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show (class Show, show)
import Data.String (localeCompare)
import Data.Ord ((>), (<))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.Argonaut.Core as J
import Data.Argonaut.Core (Json)
import Data.Int as I
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import Data.Traversable (traverse)

import Google.Sheets as GS
import Util (throwWrappedError, throwError)
import Auth (Oauth2Client)
import JsonParser (toForeign)

data Values = Values (Array (Array String))

readFoo a = do
  result <- readArray a >>= traverse readString
  pure result

readValues :: Foreign -> F Values
readValues v = do
  result <- v ! "values" >>= readArray >>= traverse readFoo
  pure $ Values result

joinTables ::
  Array (Array String) ->
  Array (Array String) ->
  Values ->
  Array (Array String)
joinTables message values (Values mapping) = sortBy
  (\a b -> second a `localeCompare` second b)
  (
    message <#>
    (\row -> concat
      [
        row `union` (first' $ filter (((==) $ first row) <<< first) mapping),
        (drop 3 $ first' $ filter (((==) $ second row) <<< second) values)
      ])
  )
  where
    first = fromMaybe "" <<< head
    first' = fromMaybe [] <<< head
    second a = fromMaybe "" $ a !! 1

positiveOrZero :: Int -> Int
positiveOrZero a
  | a > 0 = a
  | true = 0

pairsToJson :: Array (Tuple String Json) -> Json
pairsToJson = J.fromObject <<< StrMap.fromFoldable

intToJson :: Int -> Json
intToJson = J.fromNumber <<< I.toNumber

rangeToJson :: Int -> Int -> Json
rangeToJson startIndex endIndex = pairsToJson
  [
    "sheetId" `Tuple` intToJson 0,
    "dimension" `Tuple` J.fromString "ROWS",
    "startIndex" `Tuple` intToJson startIndex,
    "endIndex" `Tuple` intToJson endIndex
  ]

fitRows :: Int -> Int -> Array Json
fitRows startIndex endIndex
  | endIndex > startIndex =
    [
      pairsToJson
        [
          "insertDimension" `Tuple` pairsToJson
            [
              "range" `Tuple` rangeToJson startIndex endIndex,
              "inheritFromBefore" `Tuple` J.fromBoolean true
            ]
        ]
    ]
  | endIndex < startIndex =
      [
        pairsToJson
          [
            "insertDimension" `Tuple` pairsToJson
              [
                "range" `Tuple` rangeToJson endIndex startIndex
              ]
          ]
      ]
  | true = []

numberCellToJson :: Int -> Json
numberCellToJson value = pairsToJson
  [
    "userEnteredValue" `Tuple` pairsToJson
      [
        "numberValue" `Tuple` intToJson value
      ]
  ]

stringCellToJson :: String -> Json
stringCellToJson value = pairsToJson
  [
    "userEnteredValue" `Tuple` pairsToJson
      [
        "stringValue" `Tuple` J.fromString value
      ]
  ]

rowToJson :: Array String -> Json
rowToJson row = pairsToJson
  [
    "values" `Tuple` (J.fromArray $ cellToJson <$> row)
  ]
  where
    cellToJson cell = case I.fromString cell of
      Just n -> numberCellToJson n
      Nothing -> stringCellToJson cell

updateCells :: Array (Array String) -> Array Json
updateCells joinedTables =
  [
    pairsToJson
      [
        "updateCells" `Tuple` pairsToJson
          [
            (
              "start" `Tuple` pairsToJson
                [
                  ("sheetId" `Tuple` intToJson 0),
                  ("rowIndex" `Tuple` intToJson 1),
                  ("columnIndex" `Tuple` intToJson 0)
                ]
            ),
            ("fields" `Tuple` J.fromString "userEnteredValue"),
            ("rows" `Tuple` J.fromArray rows)
          ]
      ]
  ]
  where
    rows = rowToJson <$> joinedTables

updateSums :: Int -> Array Json
updateSums endIndex =
  [
    pairsToJson
      [
        "repeatCell" `Tuple` pairsToJson
          [
            (
              "range" `Tuple` pairsToJson
                [
                  ("sheetId" `Tuple` intToJson 0),
                  ("startRowIndex" `Tuple` intToJson 1),
                  ("endRowIndex" `Tuple` intToJson endIndex),
                  ("startColumnIndex" `Tuple` intToJson 7),
                  ("endColumnIndex" `Tuple` intToJson 8)
                ]
            ),
            (
              "cell" `Tuple` pairsToJson
                [
                  "userEnteredValue" `Tuple` pairsToJson
                    [
                      "formulaValue" `Tuple` J.fromString "=SUM(D2:G2)"
                    ]
                ]
            ),
            ("fields" `Tuple` J.fromString "userEnteredValue")
          ]
      ]
  ]

createBatchResource ::
  Array (Array String) ->
  Array (Array String) ->
  Array (Array String) ->
  Json
createBatchResource tableFromSheet tableFromEmail joinedTables =
  pairsToJson ["requests" `Tuple` J.fromArray requests]
  where
    startIndex =  positiveOrZero $ (length tableFromSheet) - 1
    endIndex = (length tableFromEmail) + 1
    requests = concat
      [
        (fitRows startIndex endIndex),
        (updateCells joinedTables),
        (updateSums endIndex)
      ]

updateSheet :: forall e.
  Oauth2Client ->
  Array (Array String) ->
  Aff (getValues :: GS.GoogleSheetsEff, err :: EXCEPTION, exception :: EXCEPTION | e) Unit
updateSheet client message =
  (attempt $ GS.getValues $ options { range = "Sheet1!A1:I" }) >>=
  (either
    (throwWrappedError "Failed to get the spreadsheet: ")
    pure
  ) >>=
  (
    (either
      (throwError
        "Failed to parse the spreadsheet: ")
      pure
    ) <<< parseValues
  ) >>=
  (\(Values v) ->
    (attempt $ GS.getValues $ options { range = "Sheet2!A1:B" }) >>=
    (either
      (throwWrappedError
        "Failed to get the mapping of the positions to the scores: ")
      pure
    ) >>=
    (
      (either
        (throwError
          "Failed to parse the mapping of the positions to the scores: ")
        (pure <<< joinTables message v)
      ) <<< parseValues
    ) >>=
    (\joinedTables -> attempt $ GS.batchUpdate
      {
        auth: client,
        spreadsheetId: sheetId,
        resource: createBatchResource v message joinedTables
      }
    ) >>=
    ((throwWrappedError "Failed to update the spreadsheet: ") `either` pure)
  )
  where
    options = { auth: client, spreadsheetId: sheetId, range: "" }
    parseValues content = runExcept $ readValues =<< toForeign (show content)
