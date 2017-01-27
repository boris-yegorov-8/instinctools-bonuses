module Token (getToken) where

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
import Control.Monad.Eff.Exception (error)
import Data.Semigroup ((<>))
import Node.ReadLine as ReadLine

import Auth as Auth
import Credentials.Token (Token)
import Util (throwError, throwWrappedError, (>>))
import Constants (tokenPath, tokenOptions)

-- (writeTextFile UTF8 Constants.tokenPath $ show token)) >> log "42"
onNewToken "" token = pure token
onNewToken errMsg _ = throwError $ "Getting new token failed: " <> errMsg

refreshToken client =
  log (
    "Authorize this app by visiting this url: " <>
    Auth.generateAuthUrl client tokenOptions
  ) >>
  (ReadLine.createConsoleInterface ReadLine.noCompletion) >>=
  (\interface ->
    (ReadLine.setPrompt "> " 2 interface) >>
    (ReadLine.setLineHandler
      interface
      (\code -> ReadLine.close interface >>
        Auth.getToken client code (\_ _ -> log "73"))
    ) >>
    (ReadLine.prompt interface))

getToken client =
  attempt (
    (readTextFile UTF8 tokenPath) >>=
    (\content -> pure $ runExcept $ readJSON content :: F Token)
  ) >>=
  (\result -> case result of
    Right (Right token) -> liftEff $ log "42" --pure token
    _ -> liftEff $ refreshToken client)
