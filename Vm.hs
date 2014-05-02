{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Vm where

import System.IO

import Control.Lens

import Data.Foldable hiding (concatMap)
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
        _machineStackS :: (Stack Val),
        _machineCP     :: [Symbol],
        _machineStackR :: (Stack Val),
        _machineMemory :: VmMemory,
        _machineEnv    :: Ptr
    } deriving (Show)
makeLenses ''MachineState

newMachine program = MachineState {
        _machineStackS = EmptyStack,
        _machineStackR = EmptyStack,
        _machineCP     = program,
        _machineMemory = mem,
        _machineEnv    = ptr}
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
    use machineCP >>= \case
        (i:instructions) -> do
            machineCP .= instructions
            return $ Just i
        [] ->
            popR >>= \case
                CP instructions' -> do
                    machineCP .= instructions'
                    takeInstruction
                Nil ->
                    return Nothing

runMachine :: Vm ()
runMachine = do
--            stack <- (gets machineStackS)
    next <- takeInstruction
    case next of
        Just i  -> do
--            liftIO $ putStr "Instruction: " >> print i 
            apply i
         {- stack <- use machineStackS
            code  <- use machineCP
            liftIO $ putStr "<<< "
            liftIO $ print code
            liftIO $ putStr "*** "
            liftIO $ print stack -}
            runMachine
        Nothing -> return ()

apply (Push x) = pushS x
apply i        = execInstruction i

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
        CP _ -> S "procedure"
        C _ ->  S "continuation"

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
        CP x  -> Str "<procedure>"
        C c   -> Str "<continuation>"
        
execInstruction If = do
    cond <- popS
    case cond of
        B c -> do _else <- popS
                  _then <- popS
                  pushS (if c then _then else _else)
        _   -> fail $ "Conditional must be boolean: " ++ show cond
               
execInstruction Call = do
    popS >>= \case 
        CP cp -> do
            returnCP <- use machineCP
            pushR $ CP returnCP
            machineCP .= cp
        C (Cont { contCP = cp,
                  contStackS = ss,
                  contStackR = sr,
                  contEnv    = env}) -> do
            popS
            arg <- popS
            machineStackS .= ss
            machineCP     .= cp
            machineEnv    .= env
            machineStackR .= sr
            pushS arg

execInstruction Jump =
    popS >>= \case
        CP cp -> machineCP .= cp
        C (Cont { contCP = cp,
                  contStackS = ss,
                  contStackR = sr,
                  contEnv    = env}) -> do
            popS
            arg <- popS
            machineStackS .= ss
            machineCP     .= cp
            machineEnv    .= env
            machineStackR .= sr
            pushS arg
        other -> fail $ "Not a valid jump destination: " ++ show other

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

execInstruction SaveCont = do
    CP  jmp <- popS 
    arg <- popS
    cp  <- use machineCP
    ss  <- use machineStackS
    sr  <- use machineStackR
    env <- use machineEnv
    
    pushS $ C $ Cont { contCP = cp, contStackS = ss, contStackR = sr, contEnv = env }
    pushS $ arg
    machineCP .= jmp

execInstruction Drop = popS >> return ()

execInstruction Cons = do
    x <- popS
    y <- popS
    newPtr <- machineMemory %%= newPair x y
    pushS $ P newPtr

execInstruction DeCons = do
    v <- popS
    case v of
        P ptr -> do
            mem <- use machineMemory
            case deref ptr mem of
                Just (Pair x y) -> do
                    pushS $ y
                    pushS $ x
                Just (Frame _ _) -> do
                    pushS $ Nil
                    pushS $ S "$vm-memory-frame"
        _ -> fail $ "Can't DeCons: " ++ show v

execInstruction SaveEnv = do
    env <- use machineEnv 
    pushS $ P env

execInstruction NewFrame = do
    parent <- popS
    ptr <- case parent of Nil   -> return $ Nothing
                          P ptr -> return $ Just ptr
                          other -> fail $ "Vm Error: Can't load frame: " ++ show other
    newPtr <- machineMemory %%= newFrame ptr
    pushS $ P newPtr

execInstruction LoadEnv = do
    popS >>= \case
        P ptr -> machineEnv .= ptr
        other -> fail $ "VM Error: failed to restore environment: " ++ (show other)

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

pushS :: Val -> Vm ()
pushS x = machineStackS %= push x

pushR :: Val -> Vm ()
pushR x = machineStackR %= push x

popS :: Vm Val
popS = machineStackS %%= popNil

popR :: Vm Val
popR = machineStackR %%= popNil

popNil stack = case pop stack of
    Just (x, s) -> (x, s)
    Nothing     -> (Nil, stack)

-- Garbage
gc :: Vm ()
gc = do
    machine <- get
    machineMemory %= sweep (marks machine)
    where marks m   = mark (getTargets (m^.machineMemory)) (stackPtrs m)
          stackPtrs m = m ^. machineEnv : getPtrs (toList (m ^. machineStackR) ++ toList (m ^. machineStackS))

getTargets :: Memory Name Val -> Ptr -> [Ptr]
getTargets mem ptr = values $ fromJust $ deref ptr mem

values (Pair v1 v2)       = getPtrs [v1, v2] 
values (Frame parent map) = maybeToList parent ++ getPtrs (elems map)

getPtrs = concatMap getPtr :: [Val] -> [Ptr]
getPtr (P p) = [p]
getPtr _     = []

-- Environment Functions
envStore key value = do
    env <- use machineEnv 
    machineMemory %= store env key value

envLookup :: Name -> Vm (Maybe Val)
envLookup key = do
    ms <- get
    return $ Memory.lookup (ms ^. machineEnv) key (ms ^. machineMemory)
