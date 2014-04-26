module Marks (mark) where 

import Control.Monad
import Control.Monad.State

type Mark a = State [a]

mark :: Eq a => (a -> [a]) -> [a] -> [a]
mark deref ptrs = execState (markPtrs deref ptrs) []

markPtrs deref ptrs = mapM_ (markPtr deref) ptrs

markPtr :: Eq a => (a -> [a]) -> a -> (Mark a) ()
markPtr deref ptr = do
    doneAlready <- doMark ptr
    if doneAlready
        then return ()
        else markPtrs deref (deref ptr)

doMark :: Eq a => a -> (Mark a) Bool
doMark x = do
    xs <- get 
    if x `elem` xs
        then return True 
        else do put $ x:xs
                return False
