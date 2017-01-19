module Main where

import Auth as Auth
import Control.Bind (bind, (>>=))
import Control.Monad.Aff (Aff, Canceler, attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Credentials.ClientSecret (ClientSecret(..))
import Credentials.Token (Token(..))
import Data.Either (Either, either)
import Data.Foreign (F, ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmptyList)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import Gmail (GmailEff, getMessages)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.ReadLine (createConsoleInterface, setPrompt, noCompletion, prompt)

type EitherClientSecret = Either (NonEmptyList ForeignError) ClientSecret
type EitherToken = Either (NonEmptyList ForeignError) Token

logError :: forall e err. (Show err) =>
  String -> err -> Eff (console :: CONSOLE | e) Unit
logError prefix = log <<< (<>) prefix <<< show

readTextFileUtf8 :: forall e.
  String -> Aff ( fs :: FS | e) (Either Error String)
readTextFileUtf8 = attempt <<< readTextFile UTF8

showMessageIds :: forall t.
    String
 -> Array {id :: String}
 -> Eff (console :: CONSOLE | t) Unit
showMessageIds "" messages = log $ show $ (\message -> message.id) <$> messages
showMessageIds err _ = log $ "Gmail API failed: " <> err

onLocalCredentialsRead :: forall e.
  String -> String -> Eff
    (console :: CONSOLE, getMessages :: GmailEff | e)
    Unit
onLocalCredentialsRead clientSecretContent tokenContent = either
  (logError "Wrong credentials: ")
  (\(ClientSecret clientSecretObject) -> either
    (logError "Authorize this app by visiting this url: ")
    (\(Token tokenObject) ->
      let
        clientWithoutToken = Auth.createClient clientSecretObject
        client = Auth.setToken tokenObject
        gmailOptions = {
          auth: client,
          userId: "me",
          q: "subject:Позиции"
        }
      in
        getMessages gmailOptions showMessageIds)
    (runExcept $ readJSON tokenContent :: F Token))
  (runExcept $ readJSON clientSecretContent :: F ClientSecret)

-- foo :: forall e. String -> Eff (console :: CONSOLE | e) Unit
foo clientSecretContent = do
  interface <- createConsoleInterface noCompletion
  setPrompt "42" 2 interface
  prompt interface
-- foo clientSecretContent = either
--   (logError "Wrong credentials: ")
--   (\(ClientSecret clientSecretObject) ->
--     let
--       oauth2client = Auth.createClient clientSecretObject
--       tokenOptions = {
--         access_type: "offline",
--         scope: "https://www.googleapis.com/auth/gmail.readonly"
--       }
--     in
--       log $
--         "Authorize this app by visiting this url: "
--         <> Auth.generateAuthUrl oauth2client tokenOptions)
--   (runExcept $ readJSON clientSecretContent :: F ClientSecret)

-- main :: forall t.
--   Eff
--     ( err :: EXCEPTION
--     , fs :: FS
--     , getMessages :: GmailEff
--     , console :: CONSOLE
--     | t
--     )
--     (Canceler
--        ( fs :: FS
--        , getMessages :: GmailEff
--        , console :: CONSOLE
--        | t
--        )
--     )
main = launchAff $ (readTextFileUtf8 clientSecretPath) >>=
  either
    (liftEff <<< logError "Loading client secret file failed: ")
    (\clientSecretContent ->
      (readTextFileUtf8 tokenPath) >>= liftEff <<< either
        (\_ -> foo clientSecretContent)
        (onLocalCredentialsRead clientSecretContent))
  where
    clientSecretPath = "./credentials/client_secret.json"
    tokenPath = "./credentials/credentials.json"
-- main = do
--   interface <- createConsoleInterface noCompletion
--   setPrompt "42" 2 interface
