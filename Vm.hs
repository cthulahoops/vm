{-# LANGUAGE FlexibleContexts #-}
module Vm where

import Data.List
import Control.Monad.State
import qualified Data.Map as M

import Debug.Trace

data Stack a = Stack a (Stack a) | EmptyStack
    deriving Show

push x stack = Stack x stack
pop (Stack x stack) = Just (x, stack)
pop EmptyStack      = Nothing
isEmpty EmptyStack  = True
isEmpty (Stack _ _) = False

data Symbol = Value Val | Instruction Ins 
    deriving (Show, Eq)

data Ins = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not | If | Call | Save | Rot | Drop | Store | Lookup | Cons | DeCons
    deriving (Show, Eq)

data Val = I Integer | B Bool | S String | CP [Symbol] Env | C Val Val | Nil
    deriving (Show, Eq)

type Env = [Table]

type Table = M.Map String Val

type Vm = StateT MachineState IO

data MachineState = MachineState {
        machineStackS :: (Stack Val),
        machineCP :: [Symbol],
        machineStackR :: (Stack Val),
        machineEnv :: Env
    } deriving (Show)

newMachine program = MachineState EmptyStack program EmptyStack envNew

runProgram program = evalStateT (runMachine >> popS) (newMachine program)

takeInstruction :: Vm (Maybe Symbol)
takeInstruction = do
    ms <- get
    case ms of 
        MachineState s (i:is) r t -> do
            put $ MachineState s is r t
            return $ Just i
        MachineState s [] r t | isEmpty r -> return Nothing
                              | otherwise -> do
                                             let Just (CP is' env, r') = pop r
                                             put $ MachineState s is' r' env
                                             takeInstruction
                           

loadInstructions :: [Symbol] -> Env -> Vm ()
loadInstructions cp env = modify (\m -> m {
        machineCP     = cp,
        machineEnv    = env ++ machineEnv m,
        machineStackR = push (CP (machineCP m) (machineEnv m)) (machineStackR m)
        })

runMachine :: Vm ()
runMachine = do
    next <- takeInstruction
    case next of
        Just i  -> do
            apply i
            runMachine
        Nothing -> return ()

apply (Value (CP symbols [])) = do
    env <- gets machineEnv
    pushS (CP symbols (M.empty:env))
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
    CP e ee <- popS
    CP t te <- popS
    B c <- popS
    pushS $ if c then CP t te else CP e ee
    execInstruction Call

execInstruction Call = do
    CP cp env <- popS
    loadInstructions cp env

execInstruction Store = do
    S key <- popS
    value <- popS
    modify (\m -> m {machineEnv = envStore key value (machineEnv m)})

execInstruction Lookup = do
    S key <- popS
    env <- gets machineEnv
    case envLookup key env of
        Just value ->
            pushS value
        Nothing ->
            fail $ "undefined variable: " ++ show key

execInstruction Save = do
    cp  <- gets machineCP
    env <- gets machineEnv
    pushR $ CP (Instruction Save : cp) env

execInstruction Drop = popS >> return ()

execInstruction Cons = do
    x <- popS
    y <- popS
    pushS $ C y x

execInstruction DeCons = do
    C x y <- popS
    pushS $ x
    pushS $ y

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

--- Environment modification:
envStore key value (e:es) = M.insert key value e : es 

envLookup key (e:es) = case M.lookup key e of
    Just value -> Just value
    Nothing    -> envLookup key es
envLookup key [] = Nothing
envNew = [M.empty]
