module Email (Message(..), getMessage) where

import Control.Monad.Aff (attempt)
import Data.Function (($))
import Data.Either (Either(..), either)
import Control.Semigroupoid ((<<<))
import Control.Bind ((>>=), (>=>))
import Data.Functor ((<$>))
import Control.Applicative (pure)
import Data.Array (last)
import Data.Maybe (maybe')
import Data.Show (class Show, show)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Data.Foreign (F)
import Control.Monad.Except (runExcept)
import Data.Foreign.Index (prop, index)

import Gmail as Gmail
import Util (throwWrappedError, throwError)
import Constants (userId)

data Message = Message String

instance showMessage :: Show Message where
  show (Message m) = m

instance messageIsForeign :: IsForeign Message where
  read =
    (prop "payload") >=>
    (prop "parts") >=>
    (index 0) >=>
    (prop "body") >=>
    (readProp "data") >=>
    (pure <<< Message)

getMessage client =
  (attempt $ Gmail.getMessages gmailOptions) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    ((maybe' (throwError "No letters were found") pure) <<<
      last <<< (<$>) (\message -> message.id))
  ) >>=
  (
    \id -> attempt $ Gmail.getMessage { auth: client, userId: userId, id: id }
  ) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    (\content -> either
      (throwError "Failed to parse the content of the email ")
      pure
      (runExcept $ readJSON (show content) :: F Message)))
  where
    gmailOptions = {
      auth: client,
      userId: userId,
      q: "subject:Позиции"
    }
