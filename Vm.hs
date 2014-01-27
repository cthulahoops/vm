{-# LANGUAGE FlexibleContexts #-}
module Vm where

import Data.List
import Control.Monad.State
import qualified Data.Map as M

import Debug.Trace

import Memory

data Stack a = Stack a (Stack a) | EmptyStack
    deriving Show

push x stack = Stack x stack
pop (Stack x stack) = Just (x, stack)
pop EmptyStack      = Nothing
isEmpty EmptyStack  = True
isEmpty (Stack _ _) = False

data Symbol = Value Val | Instruction Ins 
    deriving (Show, Eq)

data Ins = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not | If | Jmp | Save | Rot | Drop | Store | Lookup | Cons | DeCons | SaveEnv | LoadEnv | NewFrame
    deriving (Show, Eq)

data Val = I Integer | B Bool | S String | CP [Symbol] | C Val Val | P Ptr | Nil
    deriving (Show, Eq)

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
                                                                         takeInstruction
                           

loadInstructions :: [Symbol] -> Vm ()
loadInstructions cp = modify (\m -> m {
        machineCP     = cp,
        machineStackR = push (CP (machineCP m)) (machineStackR m)
        })

runMachine :: Vm ()
runMachine = do
    next <- takeInstruction
    case next of
        Just i  -> do
--            liftIO $ print i 
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

execInstruction Add  = numOp (+)
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
    pushS $ C x y

execInstruction DeCons = do
    C x y <- popS
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
    pushS $ (P newPtr)

execInstruction LoadEnv = do
    P ptr <- popS
    modify (\m -> m {machineEnv = ptr})

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

-- Environment Functions
envStore key value = modify (\m -> m {machineMemory = store (machineEnv m) key value (machineMemory m)})

envLookup :: Name -> Vm (Maybe Val)
envLookup key = do
    MachineState {machineMemory = mem, machineEnv = ptr} <- get
    return $ Memory.lookup ptr key mem
