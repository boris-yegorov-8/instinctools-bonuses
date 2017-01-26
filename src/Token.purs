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

import Auth (Oauth2Client, generateAuthUrl)
import Credentials.Token (Token(..))
import Util (throwError)
import Constants (tokenPath, tokenOptions)

refreshToken client =
  log (
    "Authorize this app by visiting this url: " <>
    generateAuthUrl client tokenOptions
  )

-- getToken :: forall e.
--   String -> Oauth2Client -> Aff (fs :: FS, console :: CONSOLE | e) String
getToken client =
  attempt (
    (readTextFile UTF8 tokenPath) >>=
    (\content -> pure $ runExcept $ readJSON content :: F Token)
  ) >>=
  (\result -> case result of
    Right (Right token) -> liftEff $ log "42" --pure token
    _ -> liftEff $ refreshToken client)
