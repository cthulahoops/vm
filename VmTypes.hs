module VmTypes where

data Symbol = Value Val | Instruction Ins 
    deriving (Show, Eq)

data Ins = Add | Mul | Sub | Flip | Dup | Gt | Lt | Eq | Ge | Le | Not | If | Jmp | Save | Rot | Drop | Store | Lookup | Cons | DeCons | SaveEnv | LoadEnv | NewFrame
    deriving (Show, Eq)

data Val = I Integer | B Bool | S String | Str String | CP [Symbol] | P Ptr | Nil
    deriving (Show, Eq)

type Ptr = Int
