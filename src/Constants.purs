module Constants (clientSecretPath, tokenPath, tokenOptions) where

clientSecretPath = "./credentials/client_secret.json" :: String
tokenPath = "./credentials/credentials.json" :: String

tokenOptions :: { access_type :: String, scope :: String }
tokenOptions = {
  access_type: "offline",
  scope: "https://www.googleapis.com/auth/gmail.readonly"
}
