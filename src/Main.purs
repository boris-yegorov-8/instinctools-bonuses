module Main where

import Auth (Options, createClient)
import Control.Bind (bind)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
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

main = (readTextFileUtf8 "./credentials/client_secret1.json") # runAff
  (log <<< ((<>) "Loading client secret file failed: ") <<< show)
  logShow
  -- tokenContent <- readTextFileUtf8 "./credentials/credentials.json"
  -- case credentialsFromJson clientSecretContent tokenContent of
  --   Tuple (Right clientSecret) (Right token) ->
  --     users
  --       (createClient $ credentialsToAuthOptions clientSecret token)
  --       logShow
  --   _ -> log "Wrong credentials"
