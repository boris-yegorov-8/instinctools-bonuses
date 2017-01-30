module Email (getMessage) where

import Control.Monad.Aff (attempt)
import Data.Function (($))

import Auth as Auth

getMessage client = attempt $ Auth.getMessages gmailOptions
  where gmailOptions = {
      auth: client,
      userId: "me",
      q: "subject:Позиции"
    }

-- showMessageIds "" messages = log $ show $ (\message -> message.id) <$> messages
-- showMessageIds err _ = log $ "Gmail API failed: " <> err
