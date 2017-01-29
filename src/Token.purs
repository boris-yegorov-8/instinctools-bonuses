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
import Node.FS.Aff (readTextFile, writeTextFile)
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
import Util (throwWrappedError)

refreshToken client =
  simpleInterface >>=
  (\interface ->
    (setPrompt (promptMessage <> "\n> ") 2 interface) *>
    (prompt interface) *>
    (setLineHandler interface) >>=
    (\code -> close interface *> pure code)
  ) >>=
  (attempt <<< Auth.getToken client) >>=
  (either
    (throwWrappedError "Getting new token failed: ")
    (liftEff <<< logShow)
  ) -- >>=
  -- (writeTextFile UTF8 tokenPath <<< show)
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
