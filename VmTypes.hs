module VmTypes where

import System.IO

type Symbol = Ins

data Ins = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not
         | If | Call | Jump | Save | Rot | Drop | Store | Lookup | Cons | DeCons
         | SaveEnv | LoadEnv | NewFrame | Type | Show | GetPort | Write | Read
         | Value Val | Error
    deriving (Show, Eq)

data Val = I Integer | B Bool | S String | Str String | CP [Ins] | P Ptr | H Handle | Nil 
    deriving (Show, Eq)

type Ptr = Int
