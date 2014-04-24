{-# LANGUAGE TemplateHaskell #-}
module Memory (
    Memory,
    Cell(..),
    newFrame,
    newMemory,
    newPair,
    deref,
    lookup,
    elems,
    sweep,
    store) where

import Prelude hiding (lookup)
import Control.Lens
import Data.Maybe
import Control.Applicative
import qualified Data.Map as M
import VmTypes

data Cell k v = Pair v v | Frame (Maybe Ptr) (M.Map k v)
    deriving (Show)

data Memory k v = Memory {
                    _memoryNext :: Ptr,
                    _memoryMap  :: M.Map Ptr (Cell k v)}
    deriving (Show)
makeLenses ''Memory

newMemory :: Memory k v
newMemory = Memory {
    _memoryNext = 0,
    _memoryMap  = M.empty}

newFrame :: Maybe Ptr -> Memory k v -> (Ptr, Memory k v)
newFrame parent memory = insert (Frame parent M.empty) memory

newPair :: v -> v -> (Memory k v) -> (Ptr, Memory k v)
newPair car cdr memory = insert (Pair car cdr) memory

insert :: Cell k v -> Memory k v -> (Ptr, Memory k v)
insert value memory = (next, over memoryNext (+1) $ over memoryMap (M.insert next value) $ memory)
    where next = memory ^. memoryNext

deref :: Ptr -> Memory k v -> Maybe (Cell k v)
deref ptr mem = M.lookup ptr (mem ^. memoryMap)

store ptr key value (Memory next mem) = Memory next (M.insert ptr (Frame parent (M.insert key value map)) mem)
    where Just (Frame parent map) = M.lookup ptr mem

lookup ptr key memory = firstJust $ M.lookup key <$> frameList ptr memory

frameList ptr memory = case M.lookup ptr (memory ^. memoryMap) of
    Nothing                        -> []
    Just (Frame (Just parent) map) -> map : frameList parent memory
    Just (Frame Nothing map)       -> [map]
    Just pair                      -> error $ "Memory Error: Expected pair got frame: " ++ show pair

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

elems = M.elems

sweep :: [Ptr] -> Memory k a -> Memory k a
sweep ptrs (Memory next map) = Memory next (M.filterWithKey isLive map)
    where isLive k _ = k `elem` ptrs

memSize (Memory next map) = M.size map
