module Credentials.Token (Token) where

data Token = Token {
  access_token :: String
  refresh_token :: String
  token_type :: String
  expiry_date :: Int
}

instance showToken :: Show Token where
  show (Token o) = fold [
      "(Token { access_token: ",
      o.access_token,
      ", refresh_token: ",
      o.refresh_token,
      ", token_type: ",
      o.token_type,
      ", expiry_date: ",
      o.expiry_date,
      " })"
    ]

-- TODO: remove duplication
instance tokenIsForeign :: IsForeign Token where
  read value = do
    access_token <- readProp "access_token" value
    refresh_token <- readProp "refresh_token" value
    token_type <- readProp "token_type" value
    expiry_date <- readProp "expiry_date" value
    pure $ Token { access_token, refresh_token, token_type, expiry_date }
