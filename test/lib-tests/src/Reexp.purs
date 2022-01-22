module Reexp (cmpArrays, module Data.Array) where

import Data.Array
import Data.Eq ((==))

cmpArrays :: Array Int -> Array Int -> Boolean
cmpArrays = (==)
