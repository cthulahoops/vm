{-# LANGUAGE FlexibleContexts #-}
module Vm where

import System.IO

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Text.Printf

import Debug.Trace

import VmTypes
import Memory
import Stack
import Marks

type Vm = StateT MachineState (ErrorT String IO)
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


evalVm machine = fst <$> runVm machine
execVm machine = snd <$> runVm machine

runVm machine = do
    result <- runErrorT $ runStateT (runMachine >> popS) machine
    case result of
        Right r ->
            return r
        Left error -> do
            liftIO $ do putStrLn "*** ERROR ***"
                        putStrLn error
            return (Nil, machine)

runProgram program = evalVm (newMachine program)

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
                                                                         -- gc
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

apply (Value x) = pushS x
apply i         = execInstruction i

execInstruction Add   = applyOp (Operator f "+")
    where f (I x) (I y)     = Just $ I (x + y)
          f (Str x) (Str y) = Just $ Str (x ++ y)
          f _ _             = Nothing

execInstruction Mul  = applyOp (makeNumOp  (*) "*")
execInstruction Sub  = applyOp (makeNumOp  (-) "-")

execInstruction Gt   = applyOp (makeBoolOp (>) ">")
execInstruction Lt   = applyOp (makeBoolOp (<) "<")
execInstruction Ge   = applyOp (makeBoolOp (>=) ">=")
execInstruction Le   = applyOp (makeBoolOp (<=) "<=")

execInstruction Eq   = applyOp (Operator f "=")
    where f x y = Just $ B $ x == y

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
    cond <- popS
    case cond of
        B c -> do _else <- popS
                  _then <- popS
                  pushS (if c then _then else _else)
        _   -> fail $ "Conditional must be boolean: " ++ show cond
               
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
    pushR $ CP (Save : cp)

execInstruction Drop = popS >> return ()

execInstruction Cons = do
    x <- popS
    y <- popS
    machine <- get 
    let (newPtr, mem') = newPair x y (machineMemory machine)
    put $ machine {machineMemory=mem'}
    pushS $ P newPtr

execInstruction DeCons = do
    v <- popS
    case v of
        P ptr -> do
            mem <- gets machineMemory
            let Just (Pair x y) = deref ptr mem
            pushS $ y
            pushS $ x
        _ -> fail $ "Can't DeCons: " ++ show v

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
    env <- popS
    case env of
        P ptr -> modify (\m -> m {machineEnv = ptr})
        other -> fail $ "VM Error: failed to restore environment: " ++ show env

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

execInstruction Error = do
    arg <- popS
    fail $ show arg

exec1 f = do
    arg <- popS 
    pushS $ f arg

exec2 f = do
    x <- popS
    y <- popS
    pushS $ f y x

data Operator = Operator (Val -> Val -> Maybe Val) String

makeNumOp f name = Operator g name
    where g (I x) (I y) = Just $ I (f x y)
          g _ _         = Nothing

makeBoolOp f name = Operator g name
    where g (I x) (I y) = Just $ B (f x y)
          g _ _         = Nothing

applyOp (Operator f name) = do
    a <- popS
    b <- popS
    case (f b a) of
        Just r  -> pushS r
        Nothing -> fail $ "Type Error: " ++ name ++ " " ++ show a ++ " " ++ show b

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
