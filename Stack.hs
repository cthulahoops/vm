module Stack where

data Stack a = Stack a (Stack a) | EmptyStack
    deriving Show

push x stack = Stack x stack
pop (Stack x stack) = Just (x, stack)
pop EmptyStack      = Nothing
isEmpty EmptyStack  = True
isEmpty (Stack _ _) = False

toList EmptyStack = []
toList (Stack head tail) = head:toList tail 
