module Main where

import Data.Function (($))
import Data.Unit (Unit)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Data.Foreign (F, ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.List.NonEmpty (NonEmptyList)
-- import Control.Semigroupoid ((<<<))
import Control.Monad.Aff (Aff, runAff)

import Credentials.ClientSecret (ClientSecret(..))
import Credentials.Token (Token(..))
import Auth (Options, createClient)
import Gmail (GmailEff, users)

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

-- main :: forall e. Eff (users :: GmailEff, console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  runAff
    logShow
    logShow
    (readTextFileUtf8 "./credentials/client_secret.json")
  -- tokenContent <- readTextFileUtf8 "./credentials/credentials.json"
  -- case credentialsFromJson clientSecretContent tokenContent of
  --   Tuple (Right clientSecret) (Right token) ->
  --     users
  --       (createClient $ credentialsToAuthOptions clientSecret token)
  --       logShow
  --   _ -> log "Wrong credentials"
