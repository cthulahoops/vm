module Memory where

import Prelude hiding (lookup)
import qualified Data.Map as M
import VmTypes

data Cell k v = Pair v v | Frame (Maybe Ptr) (M.Map k v)
    deriving (Show)

data Memory k v = Memory {
                    memoryNext :: Ptr,
                    memory     :: M.Map Ptr (Cell k v)}
    deriving (Show)

newMemory :: Memory k v
newMemory = Memory {memoryNext = 0, memory = M.empty}

newFrame :: Maybe Ptr -> Memory k v -> (Ptr, Memory k v)
newFrame parent (Memory next map) = (next, Memory {memoryNext = (next + 1), memory = (M.insert next (Frame parent M.empty) map)})

newPair :: v -> v -> (Memory k v) -> (Ptr, Memory k v)
newPair car cdr (Memory next map) = (next, Memory {memoryNext = (next + 1), memory = (M.insert next (Pair car cdr) map)})

-- deref :: Ptr -> Memory k v -> Maybe v
deref ptr mem = M.lookup ptr (memory mem)

store ptr key value (Memory next mem) = Memory next (M.insert ptr (Frame parent (M.insert key value map)) mem)
    where Just (Frame parent map) = M.lookup ptr mem

lookup ptr key m@(Memory next mem) = case M.lookup ptr mem of
                                            Just frame -> lookupFrame frame
                                            Nothing -> Nothing
    where lookupFrame (Frame parent map) = case M.lookup key map of 
                                                 Just value -> Just value
                                                 Nothing    -> case parent of
                                                        Just p' -> lookup p' key m
                                                        Nothing -> Nothing
elems = M.elems

sweep :: [Ptr] -> Memory k a -> Memory k a
sweep ptrs (Memory next map) = Memory next (M.filterWithKey isLive map)
    where isLive k _ = k `elem` ptrs

memSize (Memory next map) = M.size map
