module Sheet (Values(..), updateSheet) where

import Control.Bind ((>>=), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Data.Function (($))
import Data.Either (either)
import Data.Show (class Show, show)
import Control.Applicative (pure)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Control.Semigroupoid ((<<<))
import Control.Monad.Except (runExcept)

import GoogleSheets (getValues, batchUpdate)
import Constants (sheetId)
import Util (throwWrappedError, throwError)

data Values = Values (Array (Array String))

instance showValues :: Show Values where
  show (Values v) = show v

instance valuesIsForeign :: IsForeign Values where
  read = (readProp "values") >=> (pure <<< Values)

updateSheet client message =
  (attempt $ getValues { auth: client, spreadsheetId: sheetId, range: "A2:I" }) >>=
  (either
    (throwWrappedError "Failed to get the spreadsheet: ")
    pure
  ) >>=
  (\content -> either
    (throwError "Failed to parse the spreadsheet")
    pure
    (runExcept $ readJSON (show content) :: F Values))
  -- (\_ -> batchUpdate { auth: client, spreadsheetId: sheetId })
