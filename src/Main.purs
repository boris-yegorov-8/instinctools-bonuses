module Main where

import Auth as Auth
import Control.Bind (bind)
import Control.Monad.Aff (Aff, Canceler, attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Credentials.ClientSecret (ClientSecret(..))
import Credentials.Token (Token(..))
import Data.Either (Either(..))
import Data.Foreign (F, ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmptyList)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Gmail (GmailEff, getMessages)
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

showMessageIds :: forall t.
    String
 -> Array {id :: String}
 -> Eff (console :: CONSOLE | t) Unit
showMessageIds "" messages = log $ show $ (\message -> message.id) <$> messages
showMessageIds err _ = log $ "Gmail API failed: " <> err

onLocalCredentialsRead :: forall t e0 e1.
  ( Show e0
  , Show e1
  ) => Tuple (Either e0 ClientSecret) (Either e1 Token)
       -> Eff
            ( getMessages :: GmailEff
            , console :: CONSOLE
            | t
            )
            Unit
onLocalCredentialsRead credentials = case credentials of
  Tuple (Right (ClientSecret id secret uri)) (Right (Token tokenObject)) ->
    let
      clientWithoutToken = Auth.createClient {
        clientId: id,
        clientSecret: secret,
        redirectUri: uri
      }
      client = Auth.setToken tokenObject
      gmailOptions = {
        auth: client,
        userId: "me",
        q: "subject:Позиции"
      }
    in
      getMessages gmailOptions showMessageIds
  Tuple (Left err) _ -> log $ "Wrong credentials: " <> show err
  Tuple _ (Left err) -> log "Authorize this app by visiting this url: "

foo :: forall e. String -> Eff (console :: CONSOLE | e) Unit
foo clientSecretContent =
  case runExcept $ readJSON clientSecretContent :: F ClientSecret of
    Left err -> log $ "Wrong credentials: " <> show err
    Right (ClientSecret id secret uri) ->
      let
        oauth2Client = Auth.createClient {
          clientId: id,
          clientSecret: secret,
          redirectUri: uri
        }
        tokenOptions = {
          access_type: "offline",
          scope: "https://www.googleapis.com/auth/gmail.readonly"
        }
      in
        log $
          "Authorize this app by visiting this url: "
          <> Auth.generateAuthUrl oauth2Client tokenOptions

main :: forall t.
  Eff
    ( err :: EXCEPTION
    , fs :: FS
    , getMessages :: GmailEff
    , console :: CONSOLE
    | t
    )
    (Canceler
       ( fs :: FS
       , getMessages :: GmailEff
       , console :: CONSOLE
       | t
       )
    )
main = launchAff do
  eitherClientSecretContent <- attempt $ readTextFileUtf8 clientSecretPath
  case eitherClientSecretContent of
    Right clientSecretContent -> do
      eitherTokenContent <- attempt $ readTextFileUtf8 tokenPath
      case eitherTokenContent of
        Right tokenContent ->
          liftEff $ onLocalCredentialsRead
                  $ credentialsFromJson clientSecretContent tokenContent
        Left _ ->
          liftEff $ foo clientSecretContent
    Left err ->
      liftEff $ log $ "Loading client secret file failed: " <> show err
  where
    clientSecretPath = "./credentials/client_secret.json"
    tokenPath = "./credentials/credentials.json"
