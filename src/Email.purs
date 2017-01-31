module Email (Message(..), getMessage) where

import Control.Monad.Aff (attempt)
import Data.Function (($))
import Data.Either (Either(..), either)
import Control.Semigroupoid ((<<<))
import Control.Bind (bind, (>>=), (>=>))
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

-- TODO: try >>=
instance messageIsForeign :: IsForeign Message where
  read value =
    let
      readData =
        (prop "payload") >=>
        (prop "parts") >=>
        (index 0) >=>
        (prop "body") >=>
        (readProp "data")
    in
      do
        message <- readData value
        pure $ Message message

getMessage client =
  (attempt $ Gmail.getMessages gmailOptions) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    ((maybe' (\_ -> throwError "No letters were found") pure) <<<
      last <<< (<$>) (\message -> message.id))
  ) >>=
  (
    \id -> attempt $ Gmail.getMessage { auth: client, userId: userId, id: id }
  ) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    (\content -> either
      (\_ -> throwError "Failed to parse the content of the email ")
      pure
      (runExcept $ readJSON (show content) :: F Message)))
  where
    gmailOptions = {
      auth: client,
      userId: userId,
      q: "subject:Позиции"
    }
