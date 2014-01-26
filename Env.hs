module Env where
import Prelude hiding (lookup)
import qualified Data.Map as M

type Ptr = Int

data Frame k v = Frame (Maybe Ptr) (M.Map k v)
    deriving (Show)

data Memory k v = Memory {
                    memoryNext :: Ptr,
                    memory     :: M.Map Ptr (Frame k v)}
    deriving (Show)

data Env k v = Env {
        envPtr    :: Ptr,
        envMemory :: Memory k v}
    deriving (Show)

newMemory :: Memory k v
newMemory = Memory {memoryNext = 0, memory = M.empty}

newEnv = Env {envPtr = ptr, envMemory = mem}
    where (ptr, mem) = newFrame Nothing newMemory

newFrame :: Maybe Ptr -> Memory k v -> (Ptr, Memory k v)
newFrame parent (Memory next map) = (next, Memory {memoryNext = (next + 1), memory = (M.insert next (Frame parent M.empty) map)})

envStore  key value env = env { envMemory = store (envPtr env) key value (envMemory env) }
envLookup key env = lookup (envPtr env) key (envMemory env)
envSwitch ptr env = env {envPtr = ptr}
envFrame  maybePtr env = (newPtr, env {envMemory = mem})
    where (newPtr, mem) = newFrame maybePtr (envMemory env)

store ptr key value (Memory next mem) = Memory next (M.insert ptr (Frame parent (M.insert key value map)) mem)
    where Just (Frame parent map) = M.lookup ptr mem

lookup ptr key m@(Memory next mem) = case M.lookup ptr mem of
                                            Just (Frame parent map) ->
                                                 case M.lookup key map of 
                                                     Just value -> Just value
                                                     Nothing    -> case parent of
                                                                        Just p' -> lookup p' key m
                                                                        Nothing -> Nothing
                                            Nothing -> Nothing

test = do
    let m = newMemory :: Memory Int String
    print m
    let (a, m') = newFrame Nothing m
    print a
    let (b, m'') = newFrame (Just a) m'
    print b
    let m''' = store a 2 "Hello" m''
    return m'''
