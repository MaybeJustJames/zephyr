module Eval where

import Unsafe.Coerce (unsafeCoerce)

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
