{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module SExprs where

import Data.Monoid
import Data.List
import Data.Foldable hiding (toList, fromList)
import Data.Traversable

type SExpr = STree SValue

data STree a = SValue a | SCons (STree a) (STree a) | SNil
    deriving (Eq, Show, Functor, Traversable, Foldable)

data SValue = SSymbol String | SInt Integer | SBool Bool | SString String
    deriving (Eq, Show)

instance Monoid (STree a) where
    mempty            = SNil
    car `mappend` cdr = SCons car cdr

displayV :: SValue -> String
displayV (SSymbol x) = x
displayV (SInt x) = show x
displayV (SBool True) = "#t"
displayV (SBool False) = "#f"

display :: SExpr -> String
display  (SCons car cdr) = "(" ++ display car ++ displayTail cdr
display  SNil = "()"
display  (SValue x) = displayV x

displayTail (SCons car cdr) = " " ++ display car ++ displayTail cdr
displayTail SNil            = ")"
displayTail (SValue x)      = " . " ++ displayV x ++ ")"

mapCars :: (SExpr -> SExpr) -> SExpr -> SExpr
mapCars f (SCons car cdr) = SCons (f car) (mapCars f cdr)
mapCars f tail            = tail

toList :: SExpr -> [SExpr]
toList (SCons car cdr) = car : toList cdr
toList SNil            = []

fromList :: [SExpr] -> SExpr
fromList [] = SNil
fromList (car:cdr) = SCons car (fromList cdr)

symbol = SValue . SSymbol
strue  = SValue $ SBool True
sfalse = SValue $ SBool False

isSymbol y (SValue (SSymbol x)) = x == y
isSymbol _ _ = False
