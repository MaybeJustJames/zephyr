-- |
-- Various constants used by zephyr.
module Language.PureScript.DCE.Constants where

import Prelude hiding (maybe)

import Language.PureScript.Names

unit :: ModuleName
unit =  ModuleName "Data.Unit"

pattern Unit :: ModuleName
pattern Unit = ModuleName "Data.Unit"

semigroup :: ModuleName
semigroup = ModuleName "Data.Semigroup"

maybeMod :: ModuleName
maybeMod = ModuleName "Data.Maybe"

pattern Semigroup :: ModuleName
pattern Semigroup = ModuleName "Data.Semigroup"

semiring :: ModuleName
semiring = ModuleName "Data.Semiring"

pattern Ring :: ModuleName
pattern Ring = ModuleName "Data.Ring"

ring :: ModuleName
ring = ModuleName "Data.Ring"

pattern Semiring :: ModuleName
pattern Semiring = ModuleName "Data.Semiring"

pattern HeytingAlgebra :: ModuleName
pattern HeytingAlgebra = ModuleName "Data.HeytingAlgebra"

heytingAlgebra :: ModuleName
heytingAlgebra = ModuleName "Data.HeytingAlgebra"

pattern UnsafeCoerce :: ModuleName
pattern UnsafeCoerce = ModuleName "Unsafe.Coerce"

unsafeCoerce :: ModuleName
unsafeCoerce = ModuleName "Unsafe.Coerce"

eqMod :: ModuleName
eqMod = ModuleName "Data.Eq"

pattern Eq :: ModuleName
pattern Eq = ModuleName "Data.Eq"
