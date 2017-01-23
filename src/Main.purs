module Main where

import Auth as Auth
import Constants as Constants
import Control.Bind (class Bind, (>>=))
import Control.Monad.Aff (Aff, attempt, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Credentials.ClientSecret (ClientSecret(..))
import Credentials.Token (Token(..))
import Data.Argonaut.Core (Json)
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
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.ReadLine as ReadLine
import Client (getClient)
-- type EitherClientSecret = Either (NonEmptyList ForeignError) ClientSecret
-- type EitherToken = Either (NonEmptyList ForeignError) Token

then' :: forall m a b. (Bind m) => m a -> m b -> m b
then' ma mb = ma >>= (\_ -> mb)

infixl 1 then' as >>

-- showMessageIds :: forall t.
--     String
--  -> Array {id :: String}
--  -> Eff (console :: CONSOLE | t) Unit
-- showMessageIds "" messages = log $ show $ (\message -> message.id) <$> messages
-- showMessageIds err _ = log $ "Gmail API failed: " <> err
--
-- onLocalCredentialsRead :: forall e.
--   String -> String -> Eff
--     (console :: CONSOLE, getMessages :: GmailEff | e)
--     Unit
-- onLocalCredentialsRead clientSecretContent tokenContent = either
--   (logError "Wrong credentials: ")
--   (\(ClientSecret clientSecretObject) -> either
-- -- TODO: get new token
--     (logError "Authorize this app by visiting this url: ")
--     (\(Token tokenObject) ->
--       let
--         clientWithoutToken = Auth.createClient clientSecretObject
--         client = Auth.setToken tokenObject
--         gmailOptions = {
--           auth: client,
--           userId: "me",
--           q: "subject:Позиции"
--         }
--       in
--         getMessages gmailOptions showMessageIds)
--     (runExcept $ readJSON tokenContent :: F Token))
--   (runExcept $ readJSON clientSecretContent :: F ClientSecret)
--
-- onNewToken :: forall e.
--   String -> Json -> Eff (console :: CONSOLE, fs :: FS | e) Unit
-- onNewToken "" token =
--   (runAff
--     (logError "Getting new token failed: ")
--     (log <<< show)
--     (writeTextFile UTF8 Constants.tokenPath $ show token)) >> log "42"
-- onNewToken err _ = logError "Getting new token failed: " err
--
-- foo :: forall e.
--   String
--   -> Eff
--        ( console :: CONSOLE
--        , readline :: ReadLine.READLINE
--        , err :: EXCEPTION
--        , getToken :: Auth.AuthEff
--        , fs :: FS
--        | e
--        )
--        Unit
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
--       log (
--         "Authorize this app by visiting this url: " <>
--         Auth.generateAuthUrl oauth2client tokenOptions
--       ) >>
--       (ReadLine.createConsoleInterface ReadLine.noCompletion) >>=
--       (\interface ->
--         (ReadLine.setPrompt "> " 2 interface) >>
--         (ReadLine.setLineHandler
--           interface
--           (\code -> ReadLine.close interface >>
--             Auth.getToken oauth2client code onNewToken)) >>
--         (ReadLine.prompt interface)))
--   (runExcept $ readJSON clientSecretContent :: F ClientSecret)
--
-- main = launchAff $ (readTextFileUtf8 Constants.clientSecretPath) >>=
--   either
--     (liftEff <<< logError "Loading client secret file failed: ")
--     (\clientSecretContent ->
--       (readTextFileUtf8 Constants.tokenPath) >>= liftEff <<< either
--         (\_ -> foo clientSecretContent)
--         (onLocalCredentialsRead clientSecretContent))
main = getClient Constants.clientSecretPath >> log "73"
