module Foreign.Test where

import Prelude (Unit)

foreign import add :: Int -> Int -> Int

foreign import mult :: Int -> Int -> Int

foreign import snowflake :: String

foreign import b :: Unit -> Int
