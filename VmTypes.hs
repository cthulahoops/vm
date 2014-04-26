module VmTypes where

import System.IO
import Stack

type Symbol = Ins

data Ins = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not
         | If | Call | Jump | SaveCont | Rot | Drop | Store | Lookup | Cons | DeCons
         | SaveEnv | LoadEnv | NewFrame | Type | Show | GetPort | Write | Read
         | Value Val | Error
    deriving (Show, Eq)

data Val = I Integer | B Bool | S String | Str String | CP [Ins] | P Ptr | H Handle | C Cont | Nil 
    deriving (Show, Eq)

data Cont = Cont {
    contCP     :: [Ins],
    contStackS :: Stack Val,
    contStackR :: Stack Val,
    contEnv    :: Ptr }
    deriving (Show, Eq)

type Ptr = Int
