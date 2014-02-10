module SExprs where

import Data.List

data SExpr = SList [SExpr] | SSymbol String | SInt Integer | SBool Bool | SString String
    deriving (Eq)

instance Show SExpr where
    show (SList xs) = "(" ++ concat (intersperse " " (map show xs)) ++ ")"
    show (SSymbol x) = x
    show (SInt x) = show x
    show (SBool True) = "#t"
    show (SBool False) = "#f"
    show (SString x) = show x
    
