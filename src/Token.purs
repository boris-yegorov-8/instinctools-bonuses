module Token (getToken) where

import Control.Applicative (pure)
import Control.Bind ((>>=), (=<<))
import Control.Monad.Aff (Aff, attempt, forkAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Function (($))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile, writeTextFile)
import Data.Semigroup ((<>))
import Node.ReadLine (READLINE)
import Node.ReadLine.Aff.Simple (
  prompt,
  setPrompt,
  close,
  setLineHandler,
  simpleInterface
)
import Control.Semigroupoid ((<<<))
import Control.Apply ((*>))
import Data.Show (show)
import Control.Monad.Eff.Exception (EXCEPTION)

import Auth as Auth
import Credentials.Token (Token, readToken)
import Constants (tokenPath, tokenOptions)
import Util (throwError)
import JsonParser (toForeign)

refreshToken :: forall e.
  Auth.Oauth2Client -> Aff
    ( console :: CONSOLE
    , readline :: READLINE
    , getToken :: Auth.AuthEff
    , err :: EXCEPTION
    , fs :: FS
    , exception :: EXCEPTION
    | e
    )
    Token
refreshToken client =
  simpleInterface >>=
  (\interface ->
    (setPrompt (promptMessage <> "\n> ") 2 interface) *>
    (prompt interface) *>
    (setLineHandler interface) >>=
    (\code -> close interface *> pure code)
  ) >>=
  (attempt <<< Auth.getToken client) >>=
  (either (throwError "Wrong new token") pure) >>=
  (\token ->
    (forkAff $ writeTextFile UTF8 tokenPath $ show token) *>
    (either
      (throwError "Wrong new token")
      pure
      (runExcept $ readToken =<< toForeign (show token))))
  where
    promptMessage = "Authorize this app by visiting this url: " <>
      Auth.generateAuthUrl client tokenOptions

getToken :: forall e.
  Auth.Oauth2Client -> Aff
    ( console :: CONSOLE
    , readline :: READLINE
    , getToken :: Auth.AuthEff
    , err :: EXCEPTION
    , fs :: FS
    , exception :: EXCEPTION
    | e
    )
    Token
getToken client =
  attempt (
    (readTextFile UTF8 tokenPath) >>=
    (\content -> pure $ runExcept $ readToken =<< toForeign content)
  ) >>=
  (\result -> case result of
    Right (Right token) -> pure token
    _ -> refreshToken client)
