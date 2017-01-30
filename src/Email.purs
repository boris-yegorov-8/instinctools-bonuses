module Email (getMessage) where

import Control.Monad.Aff (attempt)
import Data.Function (($))
import Data.Either (Either(..), either)
import Control.Semigroupoid ((<<<))
import Control.Bind ((>>=))
import Data.Functor ((<$>))
import Control.Applicative (pure)

import Gmail as Gmail
import Util (throwWrappedError, throwError)

getMessage client =
  (attempt $ Gmail.getMessages gmailOptions) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    (pure <<< (<$>) (\message -> message.id))
  )
  where
    gmailOptions = {
      auth: client,
      userId: "me",
      q: "subject:Позиции"
    }
