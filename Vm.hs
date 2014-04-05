{-# LANGUAGE FlexibleContexts #-}
module Vm where

import System.IO

import Data.Maybe
import Data.List
import Control.Monad.State
import Text.Printf

import Debug.Trace

import VmTypes
import Memory
import Stack
import Marks

type Vm = StateT MachineState IO
type Name = String
type VmMemory = Memory Name Val

data MachineState = MachineState {
        machineStackS :: (Stack Val),
        machineCP     :: [Symbol],
        machineStackR :: (Stack Val),
        machineMemory :: VmMemory,
        machineEnv    :: Ptr
    } deriving (Show)

newMachine program = MachineState {
        machineStackS = EmptyStack,
        machineStackR = EmptyStack,
        machineCP     = program,
        machineMemory = mem,
        machineEnv    = ptr}
    where (ptr, mem) = newFrame Nothing newMemory

runProgram program = evalStateT (runMachine >> popS) (newMachine program)

takeInstruction :: Vm (Maybe Symbol)
takeInstruction = do
    ms <- get
    case ms of 
        MachineState {machineCP = (i:instructions)} -> do
            put $ ms {machineCP = instructions}
            return $ Just i
        MachineState {machineCP = []} -> handleReturn ms
    where handleReturn ms@(MachineState {machineStackR = r}) | isEmpty r = return Nothing
                                                             | otherwise = do
                                                                         let Just (CP instructions', r') = pop r
                                                                         put $ ms {machineCP = instructions', machineStackR = r'}
                                                                         gc
                                                                         takeInstruction
                           

loadInstructions :: [Symbol] -> Vm ()
loadInstructions cp = modify (\m -> m {
        machineCP     = cp,
        machineStackR = push (CP (machineCP m)) (machineStackR m)
        })

runMachine :: Vm ()
runMachine = do
--            stack <- (gets machineStackS)
    next <- takeInstruction
    case next of
        Just i  -> do
--            liftIO $ putStr "Instruction: " >> print i 
            apply i
--            stack <- (gets machineStackS)
--            code  <- (gets machineCP)
--            liftIO $ putStr "<<< "
--            liftIO $ print code
--            liftIO $ putStr "*** "
--            liftIO $ print stack
            runMachine
        Nothing -> return ()

apply (Value x)       = pushS x
apply (Instruction i) = execInstruction i

execInstruction Add  = do
    y <- popS
    x <- popS
    case (x, y) of
        (I x, I y)     -> pushS $ I (x + y)
        (Str x, Str y) -> pushS $ Str (x ++ y)
        _              -> fail $ "Can't add " ++ show x ++ " & " ++ show y

execInstruction Mul  = numOp (*)
execInstruction Sub  = numOp (-)
execInstruction Gt   = boolOp (>)
execInstruction Lt   = boolOp (<)
execInstruction Eq   = do
    x <- popS
    y <- popS
    pushS $ B $ x == y

execInstruction Ge   = boolOp (>=)
execInstruction Le   = boolOp (<=)
execInstruction Not  = do
    B x <- popS
    pushS $ B $ not x
execInstruction Flip = do
    x <- popS
    y <- popS
    pushS x
    pushS y
execInstruction Dup = do
    x <- popS
    pushS x
    pushS x
execInstruction Rot = do
    x <- popS
    y <- popS
    z <- popS
    pushS x
    pushS z
    pushS y
    
execInstruction Type = do
    var <- popS
    pushS $ case var of
        I _ -> S "number"
        B _ -> S "boolean"
        S _ -> S "symbol"
        Str _ -> S "string"
        Nil -> S "nil"
        P _ -> S "pair"

execInstruction Show = do
    var <- popS
    pushS $ case var of
        I x   -> Str (show x)
        Str x -> Str (show x)
        S x   -> Str x
        B True  -> Str "#t"
        B False -> Str "#f"
        Nil   -> Str "()"
        P x   -> Str ("<" ++ (show x) ++ ">")
        
execInstruction If = do
    B  c <- popS
    _else <- popS
    _then <- popS
    pushS (if c then _then else _else)

execInstruction Jmp = do
    CP cp <- popS
    loadInstructions cp

execInstruction Store = do
    S key <- popS
    value <- popS
    envStore key value

execInstruction Lookup = do
    S key <- popS
    envLookup key >>= \x -> case x of
        Just value ->
            pushS value
        Nothing ->
            fail $ "undefined variable: " ++ show key

execInstruction Save = do
    cp  <- gets machineCP
    pushR $ CP (Instruction Save : cp)

execInstruction Drop = popS >> return ()

execInstruction Cons = do
    x <- popS
    y <- popS
    machine <- get 
    let (newPtr, mem') = newPair x y (machineMemory machine)
    put $ machine {machineMemory=mem'}
    pushS $ P newPtr

execInstruction DeCons = do
    P ptr <- popS
    mem <- gets machineMemory
    let Just (Pair x y) = deref ptr mem
    pushS $ y
    pushS $ x

execInstruction SaveEnv = do
    ptr <- gets machineEnv
    pushS $ P ptr

execInstruction NewFrame = do
    parent <- popS
    let ptr = case parent of Nil   -> Nothing
                             P ptr -> Just ptr
    machine <- get
    let (newPtr, mem') = newFrame ptr (machineMemory machine)
    put $ machine {machineMemory = mem'}
    pushS $ P newPtr

execInstruction LoadEnv = do
    P ptr <- popS
    modify (\m -> m {machineEnv = ptr})

execInstruction GetPort = exec1 getPort
    where getPort (S "stdin")  = H stdin
          getPort (S "stdout") = H stdout
          getPort (S "stderr") = H stderr

execInstruction Write = do
    Str text <- popS
    H handle <- popS
    liftIO $ hPutStr handle text
    pushS $ Nil

execInstruction Read = do
    H handle <- popS
    line <- liftIO $ hGetLine handle
    pushS $ Str line

exec1 f = do
    arg <- popS 
    pushS $ f arg

exec2 f = do
    x <- popS
    y <- popS
    pushS $ f y x

numOp f = do
    I x <- popS
    I y <- popS
    pushS $ I $ f y x

boolOp f = do
    I x <- popS
    I y <- popS
    pushS $ B $ f y x

pushS x = modify (\m -> m { machineStackS = push x (machineStackS m)})
pushR x = modify (\m -> m { machineStackR = push x (machineStackR m)})

popS :: Vm Val
popS = do
    m <- get
    case pop (machineStackS m) of
        Just (x, s') -> do
            put $ m {machineStackS = s'}
            return x
        Nothing ->
            return Nil

popR :: Vm Val
popR = do
    m <- get
    let Just (x, r') = pop (machineStackR m)
    put $ m {machineStackR = r'}
    return x

-- Garbage
gc :: Vm ()
gc = modify (\machine -> machine {machineMemory = sweep (marks machine) (machineMemory machine)})
     where marks m     = mark (getTargets (machineMemory m)) (stackPtrs m)
           stackPtrs m = machineEnv m : getPtrs (toList (machineStackR m) ++ toList (machineStackS m))

getTargets :: Memory Name Val -> Ptr -> [Ptr]
getTargets mem ptr = values $ fromJust $ deref ptr mem

values (Pair v1 v2)       = getPtrs [v1, v2] 
values (Frame parent map) = maybeToList parent ++ getPtrs (elems map)

getPtrs = concatMap getPtr :: [Val] -> [Ptr]
getPtr (P p) = [p]
getPtr _     = []

-- Environment Functions
envStore key value = modify (\m -> m {machineMemory = store (machineEnv m) key value (machineMemory m)})

envLookup :: Name -> Vm (Maybe Val)
envLookup key = do
    MachineState {machineMemory = mem, machineEnv = ptr} <- get
    return $ Memory.lookup ptr key mem
