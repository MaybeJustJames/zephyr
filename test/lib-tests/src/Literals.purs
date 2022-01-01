module Literals where

data A = AStr String | AInt Int

fromAnArray :: Array A -> Array String
fromAnArray [AStr str] = [str]
fromAnArray _          = []

fromAnObject :: { a :: A } -> { a :: Array String }
fromAnObject { a : AStr str } = { a : [str] }
fromAnObject _                = { a : [] }
