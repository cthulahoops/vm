module VmTypes where

import System.IO
import Stack

type Symbol = Ins Val

data Ins a = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not
         | If | Call | Jump | SaveCont | Rot | Drop | Store | Lookup | Cons | DeCons
         | SaveEnv | LoadEnv | NewFrame | Type | Show | GetPort | Write | Read
         | Push a | Error
    deriving (Show, Eq)

data Val = I Integer | B Bool | S String | Str String | CP [Ins Val] | P Ptr | H Handle | C Cont | Nil 
    deriving (Show, Eq)

data Cont = Cont {
    contCP     :: [Symbol],
    contStackS :: Stack Val,
    contStackR :: Stack Val,
    contEnv    :: Ptr }
    deriving (Show, Eq)

type Ptr = Int
