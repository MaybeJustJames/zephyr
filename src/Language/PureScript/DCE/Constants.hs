module Language.PureScript.DCE.Constants where

import Language.PureScript.Names

unit :: ModuleName
unit =  ModuleName [ProperName "Data", ProperName "Unit"] 

pattern Unit :: ModuleName
pattern Unit = ModuleName [ProperName "Data", ProperName "Unit"]

semigroup :: ModuleName
semigroup = ModuleName [ProperName "Data", ProperName "Semigroup"] 

pattern Semigroup :: ModuleName
pattern Semigroup = ModuleName [ProperName "Data", ProperName "Semigroup"] 

semiring :: ModuleName
semiring = ModuleName [ProperName "Data", ProperName "Semiring"]

pattern Semiring :: ModuleName
pattern Semiring = ModuleName [ProperName "Data", ProperName "Semiring"]

pattern HeytingAlgebra :: ModuleName
pattern HeytingAlgebra = ModuleName [ProperName "Data", ProperName "HeytingAlgebra"]

heytingAlgebra :: ModuleName
heytingAlgebra = ModuleName [ProperName "Data", ProperName "HeytingAlgebra"]
