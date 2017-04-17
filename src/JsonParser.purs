module JsonParser (toForeign) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)

import Data.Foreign (F, Foreign, ForeignError(..), fail)

foreign import toForeignImpl :: forall r. Fn3 (String -> r) (Foreign -> r) String r

toForeign :: String -> F Foreign
toForeign json = runFn3 toForeignImpl (fail <<< JSONError) pure json
