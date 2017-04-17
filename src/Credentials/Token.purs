module Credentials.Token (Token(..), TokenObject, readToken) where

import Data.Show (class Show)
import Control.Applicative (pure)
import Data.Function (($))
import Control.Bind (bind, (>>=))
import Data.Foreign.Index ((!))
import Data.Foreign (F, Foreign, readString, readNumber)

type TokenObject = {
  access_token :: String,
  refresh_token :: String,
  token_type :: String,
  expiry_date :: Number
}

data Token = Token TokenObject

readToken :: Foreign -> F Token
readToken value = do
  access_token <- value ! "access_token" >>= readString
  refresh_token <- value ! "refresh_token" >>= readString
  token_type <- value ! "token_type" >>= readString
  expiry_date <- value ! "expiry_date" >>= readNumber
  pure $ Token { access_token, refresh_token, token_type, expiry_date }
