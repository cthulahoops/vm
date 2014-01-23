{-# LANGUAGE FlexibleInstances #-}
module Assemble where

import Vm
import GHC.Exts

instance IsString [Symbol] where
    fromString = assemble

data Token = Start | End | Sym Symbol
    deriving Show

run = runProgram . assemble 

assemble text = program
    where (program, []) = (lexer . tokenize) text

tokenize = map toToken . words

lexer (Sym s:xs) = let (this, rest) = lexer xs in (s:this, rest)
lexer (Start:xs) = (Value (CP this []):that, rest')
    where (this, rest)  = lexer xs
          (that, rest') = lexer rest
lexer (End:xs) = ([], xs)
lexer [] = ([], [])

toToken "[" = Start
toToken "]" = End
toToken x = Sym $ toSymbol x

toSymbol "+" = Instruction Add
toSymbol "-" = Instruction Sub
toSymbol "*" = Instruction Mul
toSymbol ">" = Instruction Gt
toSymbol ">=" = Instruction Ge
toSymbol "<=" = Instruction Le
toSymbol "<" = Instruction Lt
toSymbol "=" = Instruction Eq
toSymbol "dup" = Instruction Dup
toSymbol "flip" = Instruction Flip
toSymbol "def" = Instruction Store
toSymbol "!" = Instruction Lookup
toSymbol "rot" = Instruction Rot
toSymbol "call" = Instruction Call
toSymbol "not" = Instruction Not
toSymbol "if" = Instruction If
toSymbol "drop" = Instruction Drop
toSymbol "," = Instruction Cons
toSymbol "`" = Instruction DeCons
toSymbol s@(x:xs) | x >= '0' && x <= '9' = Value $ I $ read s
toSymbol "true" = Value $ B True
toSymbol "false" = Value $ B False
toSymbol "nil"   = Value $ Nil
toSymbol (':':xs) = Value $ S xs
