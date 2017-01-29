module Token (getToken, refreshToken) where

import Control.Applicative (pure)
import Control.Bind ((>>=), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Function (($))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Control.Monad.Eff.Exception (error, throwException)
import Data.Semigroup ((<>))
import Node.ReadLine.Aff.Simple (
  prompt,
  setPrompt,
  close,
  setLineHandler,
  simpleInterface
)
import Control.Semigroupoid ((<<<))
import Control.Apply ((*>))

import Auth as Auth
import Credentials.Token (Token)
import Constants (tokenPath, tokenOptions)

-- (writeTextFile UTF8 Constants.tokenPath $ show token)) >> log "42"
onNewToken "" token = logShow token -- pure token
onNewToken errMsg _ = throwException $ error $ "Getting new token failed: " <> errMsg

refreshToken client =
  simpleInterface >>=
  (\interface ->
    (setPrompt (promptMessage <> "\n> ") 2 interface) *>
    (prompt interface) *>
    (setLineHandler interface) >>=
    (\code -> close interface *> pure code)
  ) >>=
  (\code -> liftEff $ Auth.getToken client code onNewToken)
  where
    promptMessage = "Authorize this app by visiting this url: " <>
      Auth.generateAuthUrl client tokenOptions

getToken client =
  attempt (
    (readTextFile UTF8 tokenPath) >>=
    (\content -> pure $ runExcept $ readJSON content :: F Token)
  ) >>=
  (\result -> case result of
    Right (Right token) -> liftEff $ log "42" --pure token
    _ -> refreshToken client)
