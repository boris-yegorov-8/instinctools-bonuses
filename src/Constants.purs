module Constants (clientSecretPath, tokenPath, tokenOptions, userId) where

clientSecretPath = "./credentials/client_secret.json" :: String
tokenPath = "./credentials/credentials.json" :: String
userId = "me" :: String

tokenOptions :: { access_type :: String, scope :: String }
tokenOptions = {
  access_type: "offline",
  scope: "https://www.googleapis.com/auth/gmail.readonly"
}
