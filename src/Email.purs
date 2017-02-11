module Email (Message(..), getMessage) where

import Node.Buffer as Buffer
import Control.Applicative (pure)
import Control.Bind ((>>=), (>=>))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Array (last, filter, head)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Data.Foreign.Index (prop, index)
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.Maybe (maybe', fromMaybe)
import Data.Show (class Show, show)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(..))
import Control.Monad.Eff (Eff)
import Data.String(Pattern(..), split, contains)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex as Regex
import Control.Monad.Eff.Exception (EXCEPTION)

import Auth (Oauth2Client)
import Google.Gmail as Gmail
import Util (throwWrappedError, throwError)
import Constants (userId)

data Message = Message String

instance showMessage :: Show Message where
  show (Message m) = m

instance messageIsForeign :: IsForeign Message where
  read =
    (prop "payload") >=>
    (prop "parts") >=>
    (index 0) >=>
    (prop "body") >=>
    (readProp "data") >=>
    (pure <<< Message)

changeStringEncoding :: forall e.
  Encoding -> Encoding -> String -> Eff (buffer :: BUFFER | e) String
changeStringEncoding from to =
  (flip Buffer.fromString) from >=> Buffer.toString to

parseLine :: Array String -> Array String
parseLine line =
  [
    (Regex.replace pattern0 "" $ fromMaybe "" $ last line),
    (Regex.replace pattern1 "" $ fromMaybe "" $ head line)
  ]
  where
    pattern0 = unsafeRegex " [0-9]$" RegexFlags.global
    pattern1 = unsafeRegex "^[0-9]+ " RegexFlags.global

parseContent :: forall e.
  String ->
  Eff( buffer :: BUFFER, err :: EXCEPTION | e) (Array (Array String))
parseContent =
  (changeStringEncoding Base64 UTF8) >=>
  (
    (\lines -> maybe'
      (throwError "No info in the email")
      (\_ -> pure lines)
      (head lines)
    ) <<<
    ((<$>) parseLine) <<<
    ((<$>) $ split $ Pattern " УУ: ") <<<
    (filter $ contains $ Pattern " УУ: ") <<<
    (split $ Pattern "\r\n")
  )

getMessage :: forall e.
  Oauth2Client ->
  Aff
    (getMessages :: Gmail.GmailEff, err :: EXCEPTION, buffer :: BUFFER | e)
    (Array (Array String))
getMessage client =
  (attempt $ Gmail.getMessages gmailOptions) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    ((maybe' (throwError "No letters were found") pure) <<<
      last <<< (<$>) (\message -> message.id))
  ) >>=
  (
    \id -> attempt $ Gmail.getMessage { auth: client, userId: userId, id: id }
  ) >>=
  (either
    (throwWrappedError "Gmail API failed: ")
    (\content -> either
      (throwError "Failed to parse the content of the email ")
      (liftEff <<< parseContent <<< show)
      (runExcept $ readJSON (show content) :: F Message)))
  where
    gmailOptions = {
      auth: client,
      userId: userId,
      q: "subject:Позиции"
    }
