module SExprs where

import Data.List

data SExpr = SList [SExpr] | SSymbol String | SInt Integer | SBool Bool | SString String
    deriving (Eq, Show)

display (SList xs) = "(" ++ concat (intersperse " " (map display xs)) ++ ")"
display (SSymbol x) = x
display (SInt x) = show x
display (SBool True) = "#t"
display (SBool False) = "#f"
display (SString x) = show x
