-- | Various constants used by zephyr.
--
module Language.PureScript.DCE.Constants where

import Prelude hiding (maybe)

import Language.PureScript.Names


unit :: ModuleName
unit =  ModuleName "Data.Unit"

pattern Unit :: ModuleName
pattern Unit = ModuleName "Data.Unit"

maybeMod :: ModuleName
maybeMod = ModuleName "Data.Maybe"

semigroup :: ModuleName
semigroup = ModuleName "Data.Semigroup"

pattern Semigroup :: ModuleName
pattern Semigroup = ModuleName "Data.Semigroup"

semiring :: ModuleName
semiring = ModuleName "Data.Semiring"

ring :: ModuleName
ring = ModuleName "Data.Ring"

pattern Ring :: ModuleName
pattern Ring = ModuleName "Data.Ring"

pattern Semiring :: ModuleName
pattern Semiring = ModuleName "Data.Semiring"

heytingAlgebra :: ModuleName
heytingAlgebra = ModuleName "Data.HeytingAlgebra"

pattern HeytingAlgebra :: ModuleName
pattern HeytingAlgebra = ModuleName "Data.HeytingAlgebra"

unsafeCoerce :: ModuleName
unsafeCoerce = ModuleName "Unsafe.Coerce"

pattern UnsafeCoerce :: ModuleName
pattern UnsafeCoerce = ModuleName "Unsafe.Coerce"

eqMod :: ModuleName
eqMod = ModuleName "Data.Eq"

pattern Eq :: ModuleName
pattern Eq = ModuleName "Data.Eq"
