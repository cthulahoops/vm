module SExprs where

import Data.List

data SExpr = SSymbol String | SInt Integer | SBool Bool | SString String | SNil | SCons SExpr SExpr
    deriving (Eq, Show)

display (SCons car cdr) = "(" ++ display car ++ displayTail cdr
display (SSymbol x) = x
display (SInt x) = show x
display (SBool True) = "#t"
display (SBool False) = "#f"
display SNil = "()"
display (SString x) = show x

displayTail (SCons car cdr) = " " ++ display car ++ displayTail cdr
displayTail SNil = ")"
displayTail other = " . " ++ display other ++ ")"

mapCars :: (SExpr -> SExpr) -> SExpr -> SExpr
mapCars f (SCons car cdr) = SCons (f car) (mapCars f cdr)
mapCars f tail            = tail

toList :: SExpr -> [SExpr]
toList (SCons car cdr) = car : toList cdr
toList SNil            = []

fromList :: [SExpr] -> SExpr
fromList [] = SNil
fromList (car:cdr) = SCons car (fromList cdr)
