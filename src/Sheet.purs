module Sheet (Values(..), updateSheet) where

import Constants (sheetId)
import Control.Applicative (pure)
import Control.Bind ((>>=), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Array (init)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Data.Function (($))
import Data.Maybe (maybe, fromMaybe)
import Data.Show (class Show, show)
import GoogleSheets (getValues, batchUpdate)
import Util (throwWrappedError, throwError)

data Values = Values (Array (Array String))

instance showValues :: Show Values where
  show (Values v) = show v

instance valuesIsForeign :: IsForeign Values where
  read = (readProp "values") >=> (pure <<< Values)

parseValues content = runExcept $ readJSON (show content) :: F Values

foo message values mapping = fromMaybe [] $ init values

updateSheet client message =
  (attempt $ getValues $ options { range = "Sheet1!A2:I" }) >>=
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
        (pure <<< foo message v)
      ) <<< parseValues
    )
  )
  where
    options = { auth: client, spreadsheetId: sheetId, range: "" }
  -- (\_ -> batchUpdate { auth: client, spreadsheetId: sheetId })
