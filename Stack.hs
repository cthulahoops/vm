{-# LANGUAGE DeriveFoldable #-}
module Stack where

import Data.Foldable

data Stack a = Stack a (Stack a) | EmptyStack
    deriving (Show, Eq, Foldable)

push x stack = Stack x stack
pop (Stack x stack) = Just (x, stack)
pop EmptyStack      = Nothing
isEmpty EmptyStack  = True
isEmpty (Stack _ _) = False
