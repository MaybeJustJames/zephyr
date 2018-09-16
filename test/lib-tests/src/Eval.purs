module Eval where

import Data.Maybe (Maybe)
import Data.Array ((!!))
import Data.Eq ((==))
import Unsafe.Coerce (unsafeCoerce)

evalVars :: Boolean
evalVars =
  let a = false
      b = false 
  in a == b

evalUnderArrayLiteral :: Array (Maybe Boolean)
evalUnderArrayLiteral =
  let a = [true, false] !! 0
  in [a]

evalUnderObjectLiteral :: { a :: Maybe Boolean }
evalUnderObjectLiteral =
  let a = [true, false] !! 0
  in { a }

type Update i s
  = { interpret :: i, status :: s }
  -> { interpret :: i, status :: s }

-- |
-- issue #14 `Spork.App.makeAppQueue` diverging
makeAppQueue
  :: forall i s.
   { update :: Update i s
   , commit :: Update i s
   }
makeAppQueue =
  let
    update :: Update i s
    update state@{ interpret: _ } =
        let status    = unsafeCoerce {}
            nextState = state { status = status }
        in nextState

    commit :: Update i s
    commit state =
        let status    = unsafeCoerce {}
            nextState = state { status = status }
        in nextState

  in { update, commit }

data Action = Foo String | Bar Int
type State = { foo :: String, bar :: Int }

recordUpdate :: State -> Action -> State
recordUpdate state = case _ of
  Foo str -> state { foo = str }
  Bar n -> state { bar = n }
