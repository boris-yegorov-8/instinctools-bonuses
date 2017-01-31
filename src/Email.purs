module Email (getMessage) where

import Control.Monad.Aff (attempt)
import Data.Function (($))
import Data.Either (Either(..), either)
import Control.Semigroupoid ((<<<))
import Control.Bind ((>>=))
import Data.Functor ((<$>))
import Control.Applicative (pure)
import Data.Array (last)
import Data.Maybe (maybe')

import Gmail as Gmail
import Util (throwWrappedError, throwError)
import Constants (userId)

getMessage client =
  (attempt $ Gmail.getMessages gmailOptions) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    ((maybe' (\_ -> throwError "No letters were found") pure) <<<
      last <<< (<$>) (\message -> message.id))
  ) >>=
  (\id -> Gmail.getMessage { auth: client, userId: userId, id: id })
  where
    gmailOptions = {
      auth: client,
      userId: userId,
      q: "subject:Позиции"
    }
