module Compile where

import Data.List
import Assemble

data SExpr = SExpr [SExpr] | SSymbol String | SInt Integer
    deriving Show

lisp = run . compile . parse

example1 = SInt 42
example2 = SExpr [SSymbol "+", SInt 7, SInt 3]
example3 = SExpr [SSymbol "+", SInt 7, SExpr [SSymbol "-", SInt 6, SInt 2]]
example4 = SExpr [SSymbol "+", SInt 4, SInt 3, SInt 2, SInt 1]

compile = concat . intersperse " " . optimise . concat . map compileTokens

compileTokens (SInt x)       = [show x]
compileTokens (SExpr [(SSymbol "define"), SSymbol name, body]) = compileTokens body ++ [":" ++ name, "def"]
compileTokens (SExpr [(SSymbol "lambda"), SExpr vars, body]) = ["["] ++ reverse [":" ++ x ++ " def" | SSymbol x <- vars] ++ compileTokens body ++ ["]"]
compileTokens (SExpr [(SSymbol "if"), cond, true_branch, false_branch]) = compileTokens cond ++ ["["] ++ compileTokens true_branch ++ ["]"] ++ ["["] ++ compileTokens false_branch ++ ["]"] ++ ["if"]
compileTokens (SExpr (x:xs)) = concat (map compileTokens xs) ++ compileTokens x ++ ["call"]
compileTokens (SExpr [])     = [] 
compileTokens (SSymbol x)    = [":" ++ x, "!"]

optimise (":=":"!":"call":xs) = "!":optimise xs
optimise (":<":"!":"call":xs) = "<":optimise xs
optimise (":>":"!":"call":xs) = ">":optimise xs
optimise (":-":"!":"call":xs) = "-":optimise xs
optimise (":+":"!":"call":xs) = "+":optimise xs
optimise (":*":"!":"call":xs) = "+":optimise xs
optimise (":/":"!":"call":xs) = "+":optimise xs
optimise (x:xs) = x:optimise xs
optimise [] = []

-- Parser - probably separate.
parse = fst . parse'

parse' :: String -> ([SExpr], String)
parse' ""       = ([], [])
parse' ('(':xs) = (SExpr this:that, rest')
    where (this, rest)  = parse' xs
          (that, rest') = parse' rest
parse' (')':xs) = ([], xs)
parse' (' ':xs) = parse' xs
parse' xs = (parseSymbol this:that, rest')
    where (this, rest)  = splitToken xs
          (that, rest') = parse' rest          

parseSymbol xs@(x:_) | x >='0' && x <= '9' = SInt (read xs)
                     | otherwise           = SSymbol xs

splitToken (' ':xs) = ("", xs)
splitToken (')':xs) = ("", ')':xs)
splitToken (x:xs) = (x:this, rest)
    where (this, rest) = splitToken xs
