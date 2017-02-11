module Sheet (Values(..), updateSheet) where

import Constants (sheetId)
import Control.Alt (alt)
import Control.Applicative (pure)
import Control.Bind ((>>=), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Array (length, concat, drop, filter, head, sortBy, union, (!!))
import Data.Either (either)
import Data.Eq ((==))
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Data.Function (($))
import Data.Functor ((<#>))
import Data.Maybe (maybe, fromMaybe)
import Data.Show (class Show, show)
import Data.String (localeCompare)
import Data.Ord ((>), (<))
import Data.Ring ((-))
import Data.Int (toNumber)
import Data.Semiring ((+))

import GoogleSheets (Request(..), getValues, batchUpdate)
import Util (throwWrappedError, throwError)

data Values = Values (Array (Array String))

instance showValues :: Show Values where
  show (Values v) = show v

instance valuesIsForeign :: IsForeign Values where
  read = (readProp "values") >=> (pure <<< Values)

parseValues content = runExcept $ readJSON (show content) :: F Values

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

positiveOrZero a
  | a > 0 = a
  | true = 0

-- fitRows :: forall options. Int -> Int -> { | options }
fitRows startIndex endIndex
  | endIndex > startIndex =
    [
      InsertDimension
        {
          insertDimension:
            {
              range:
                {
                  sheetId: toNumber 0,
                  dimension: "ROWS",
                  startIndex: toNumber startIndex,
                  endIndex: toNumber endIndex
                },
              inheritFromBefore: true
            }
        }
    ]
  | endIndex < startIndex =
    [
      DeleteDimension
        {
          deleteDimension:
            {
              range:
                {
                  sheetId: toNumber 0,
                  dimension: "ROWS",
                  startIndex: toNumber endIndex,
                  endIndex: toNumber startIndex
                }
            }
        }
    ]
  | true = []

-- createBatchResource :: forall options. Array (Array String) -> Array (Array String) -> (| options)
createBatchResource tableFromSheet tableFromEmail =
  {
    requests: (fitRows startIndex endIndex)
      -- [
      --   (fitRows tableFromSheet tableFromEmail)
      -- ]
  }
  where
    startIndex =  positiveOrZero $ (length tableFromSheet) - 1
    endIndex = (length tableFromEmail) + 1

updateSheet client message =
  (attempt $ getValues $ options { range = "Sheet1!A1:I" }) >>=
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
    (attempt $ getValues $ options { range = "Sheet2!A1:B" }) >>=
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
    (\_ -> attempt $ batchUpdate
      {
        auth: client,
        spreadsheetId: sheetId,
        resource: createBatchResource v message
      }
    )
  )
  where
    options = { auth: client, spreadsheetId: sheetId, range: "" }
