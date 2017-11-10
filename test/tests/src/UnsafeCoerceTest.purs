module Unsafe.Coerce.Test where

import Unsafe.Coerce (unsafeCoerce)

data X = X Number Number

unsafeX :: Int -> Int -> X
unsafeX = unsafeCoerce X
