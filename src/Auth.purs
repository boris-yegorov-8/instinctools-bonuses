module Auth (createClient) where

foreign import createClient :: String -> { a :: Number, b :: Number }
