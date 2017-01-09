module Main where

import Auth (Options, createClient)
import Control.Bind (bind)
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Credentials.ClientSecret (ClientSecret(..))
import Credentials.Token (Token(..))
import Data.Either (Either(..))
import Data.Foreign (F, ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Function (($), (#))
import Data.List.NonEmpty (NonEmptyList)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Gmail (GmailEff, users)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)

type EitherClientSecret = Either (NonEmptyList ForeignError) ClientSecret
type EitherToken = Either (NonEmptyList ForeignError) Token

readTextFileUtf8 :: forall eff. String -> Aff (fs :: FS | eff) String
readTextFileUtf8 = readTextFile UTF8

credentialsFromJson :: String -> String -> Tuple EitherClientSecret EitherToken
credentialsFromJson clientSecretContent tokenContent = Tuple
  (runExcept $ readJSON clientSecretContent :: F ClientSecret)
  (runExcept $ readJSON tokenContent :: F Token)

credentialsToAuthOptions :: ClientSecret -> Token -> Options
credentialsToAuthOptions (ClientSecret id secret uri) (Token t) = {
  clientId: id,
  clientSecret: secret,
  redirectUri: uri,
  token: t
}

foo credentials = case credentials of
  Tuple (Right clientSecret) (Right token) ->
    users
      (createClient $ credentialsToAuthOptions clientSecret token)
      logShow
  Tuple (Left err) _ -> log ("Wrong credentials: " <> (show err))
  Tuple _ (Left err) -> log ("Wrong credentials: " <> (show err))

main = launchAff do
  eitherClientSecretContent <- attempt $ readTextFileUtf8 clientSecretPath
  case eitherClientSecretContent of
    Right clientSecretContent -> do
      eitherTokenContent <- attempt $ readTextFileUtf8 tokenPath
      case eitherTokenContent of
        Right tokenContent ->
          liftEff $ foo $ credentialsFromJson clientSecretContent tokenContent
        Left _ -> liftEff $ log "Authorize this app by visiting this url: "
    Left err ->
      liftEff $ log ("Loading client secret file failed: " <> (show err))
  where
    clientSecretPath = "./credentials/client_secret.json"
    tokenPath = "./credentials/credentials.json"
