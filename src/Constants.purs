module Constants (
  clientSecretPath,
  tokenPath,
  tokenOptions,
  userId,
  sheetId
) where

clientSecretPath = "./credentials/client_secret.json" :: String
tokenPath = "./credentials/credentials.json" :: String
userId = "me" :: String
sheetId = "1iVqf1jtyQF55Nht5NVQvGRYKMMkJhyjYE0ordl8mim8" :: String

tokenOptions :: { access_type :: String, scope :: Array String }
tokenOptions = {
  access_type: "offline",
  scope:
    [
      "https://www.googleapis.com/auth/gmail.readonly",
      "https://www.googleapis.com/auth/spreadsheets"
    ]
}
