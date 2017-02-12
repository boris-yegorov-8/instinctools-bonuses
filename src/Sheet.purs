module Sheet (Values(..), updateSheet) where

import Constants (sheetId)
import Control.Applicative (pure)
import Control.Bind ((>>=), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Array (
  length,
  concat,
  take,
  drop,
  filter,
  head,
  sortBy,
  union,
  init,
  last,
  (!!)
)
import Data.Either (either)
import Data.Eq ((==))
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Data.Function (($))
import Data.Functor ((<#>), (<$>))
import Data.Maybe (maybe, fromMaybe)
import Data.Show (class Show, show)
import Data.String (localeCompare)
import Data.Ord ((>), (<))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.HeytingAlgebra ((&&))
import Data.Semigroup ((<>))
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex as Regex

import Google.Sheets as GS
import Util (throwWrappedError, throwError)

data Values = Values (Array (Array String))

instance showValues :: Show Values where
  show (Values v) = show v

instance valuesIsForeign :: IsForeign Values where
  read = (readProp "values") >=> (pure <<< Values)

parseValues content = runExcept $ readJSON (show content) :: F Values

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

fitRows :: Int -> Int -> Array String
fitRows startIndex endIndex
  | endIndex > startIndex =
    [
      """
        {
          "insertDimension":
            {
              "range":
                {
                  "sheetId": 0,
                  "dimension": "ROWS",
                  "startIndex": """ <> show startIndex <> """,
                  "endIndex": """ <> show endIndex <> """
                },
              "inheritFromBefore": true
            }
        }
      """
    ]
  | endIndex < startIndex =
    [
      """
        {
          "deleteDimension":
            {
              "range":
                {
                  "sheetId": 0,
                  dimension: "ROWS",
                  "startIndex": """ <> show endIndex <> """,
                  "endIndex": """ <> show startIndex <> """
                }
            }
        }
      """
    ]
  | true = []

numberCell value =
  """
    {
      "userEnteredValue": { "numberValue": """ <> value <> """ }
    }
  """

stringCell value =
  """
    {
      "userEnteredValue": { "stringValue": """ <> value <> """ }
    }
  """

rowValues row = show $ concat
  [
    (stringCell <$> (take 2 row)),
    (numberCell <$> (2 `drop` [] `fromMaybe` init row)),
    (stringCell <$> (["" `fromMaybe` last row]))
  ]

updateCells :: Array (Array String) -> Array String
updateCells joinedTables =
  [
    """
      {
        "updateCells":
          {
            "start":
              {
                "sheetId": 0,
                rowIndex: 1,
                columnIndex: 0
              },
            "rows": """ <> show rows <> """,
            fields: "userEnteredValue"
          }
      }
    """
  ]
  where
    rows = (1 `take` joinedTables) <#>
      (\row -> """{ "values": """ <> rowValues row <> """ }""")

-- fitSums startIndex endIndex
--   | (startIndex > 1) && (endIndex > startIndex) =
--     [
--       CopyPaste
--         {
--           copyPaste:
--             {
--               source:
--                 {
--                   sheetId: toNumber 0,
--                   startRowIndex: toNumber $ startIndex,
--                   endRowIndex: toNumber $ startIndex + 1,
--                   startColumnIndex: toNumber 7,
--                   endColumnIndex: toNumber 8
--                 },
--               destination:
--                 {
--                   sheetId: toNumber 0,
--                   startRowIndex: toNumber $ startIndex + 1,
--                   endRowIndex: toNumber endIndex,
--                   startColumnIndex: toNumber 7,
--                   endColumnIndex: toNumber 8
--                 },
--               pasteType: "PASTE_FORMULA"
--             }
--         }
--     ]
--   | true = []

createBatchResource ::
  Array (Array String) ->
  Array (Array String) ->
  Array (Array String) ->
  String
createBatchResource tableFromSheet tableFromEmail joinedTables =
    ("""{ "requests": """ <> requests <> """ }""")

  where
    startIndex =  positiveOrZero $ (length tableFromSheet) - 1
    endIndex = (length tableFromEmail) + 1
    requests = --Regex.replace
      -- (unsafeRegex "\n" RegexFlags.global)
      (show $ concat
        [
          ["42"]
          -- (fitRows startIndex endIndex)
          -- (updateCells joinedTables)
          -- (fitSums startIndex endIndex)
        ])

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
