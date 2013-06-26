{-# LANGUAGE FlexibleContexts #-}
module Vm where

import Data.List
import Control.Monad.State

import Debug.Trace

data Stack a = Stack a (Stack a) | EmptyStack
    deriving Show

push x stack = Stack x stack
pop (Stack x stack) = (x, stack)
isEmpty EmptyStack  = True
isEmpty (Stack _ _) = False

data Symbol = Value Val | Instruction Ins 
    deriving Show

data Ins = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not | If | Call | Save | Rot | DropR
    deriving Show

data Val = I Integer | B Bool | S String | CP [Symbol]
    deriving Show

-- exec = fst . pop . execute . tokenize
-- run = execute . tokenize

tokenize = map toToken . words
toToken "+" = Instruction Add
toToken "-" = Instruction Sub
toToken "*" = Instruction Mul
toToken ">" = Instruction Gt
toToken ">=" = Instruction Ge
toToken "<=" = Instruction Le
toToken "<" = Instruction Lt
toToken "=" = Instruction Eq
toToken "dup" = Instruction Dup
toToken "flip" = Instruction Flip
toToken "not" = Instruction Not
toToken "if" = Instruction If
toToken s@(x:xs) | x >= '0' && x <= '9' = Value $ I $ read s
toToken "true" = Value $ B True
toToken "false" = Value $ B False
toToken (':':xs) = Value $ S xs

-- execute p = foldl' apply EmptyStack p

type Vm = StateT MachineState IO

data MachineState = MachineState {
        machineStackS :: (Stack Val),
        machineCP :: [Symbol],
        machineStackR :: (Stack Val)
    } deriving (Show)

runProgram program = execStateT runMachine (MachineState EmptyStack program EmptyStack)

takeInstruction :: Vm (Maybe Symbol)
takeInstruction = do
    ms <- get
    case ms of 
        MachineState s (i:is) r -> do
            put $ MachineState s is r
            return $ Just i
        MachineState s [] r | isEmpty r -> return Nothing
                            | otherwise -> do
                                             let (CP is', r') = pop r
                                             put $ MachineState s is' r'
                                             takeInstruction
                           

loadInstructions :: [Symbol] -> Vm ()
loadInstructions cp = do
    MachineState s is r <- get
    put $ MachineState s cp (push (CP is) r)

runMachine :: Vm ()
runMachine = do
    next <- takeInstruction
    case next of
        Just i  -> do
            liftIO $ putStr "Instruction: "
            liftIO $ print i
            apply i
            (get >>= liftIO . print)
            runMachine
        Nothing -> return ()

apply (Value x)       = pushS x
apply (Instruction i) = execInstruction i

execInstruction Add  = numOp (+)
execInstruction Mul  = numOp (*)
execInstruction Sub  = numOp (-)
execInstruction Gt   = boolOp (>)
execInstruction Lt   = boolOp (<)
execInstruction Eq   = boolOp (==)
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
    CP e <- popS
    CP t <- popS
    B c <- popS
    pushS $ if c then CP t else CP e 
    (get >>= liftIO . print)
    execInstruction Call

execInstruction Call = do
    CP cp <- popS
    loadInstructions cp

execInstruction Save = do
    cp <- gets machineCP
    pushR $ CP (Instruction Save : cp)

execInstruction DropR = popR >> return ()

numOp f = do
    I x <- popS
    I y <- popS
    pushS $ I $ f y x

boolOp f = do
    I x <- popS
    I y <- popS
    pushS $ B $ f y x

pushS x = modify (\(MachineState s is r) -> MachineState (push x s) is r)
pushR x = modify (\(MachineState s is r) -> MachineState s is (push x r))

popS :: Vm Val
popS = do
    MachineState s is r <- get
    let (x, s') = pop s
    put $ MachineState s' is r 
    return x

popR :: Vm Val
popR = do
    MachineState s is r <- get
    let (x, r') = pop r
    put $ MachineState s is r' 
    return x
