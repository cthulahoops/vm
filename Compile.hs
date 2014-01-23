module Compile where

import Control.Applicative
import Data.List
import Assemble
import Vm
import Control.Monad.State
import System.Console.Readline

data SExpr = SExpr [SExpr] | SSymbol String | SInt Integer | SQuote SExpr
    deriving Show

repl = do
        core <- readFile "lib/core.s"
        machine <- execStateT runMachine (newMachine (assemble core))
        loop machine
    where loop machine = do
                Just code <- readline ">>> "
                let program = (assemble.compile.parse) code
                (result, machine'') <- runStateT (runMachine >> popS) (machine {machineCP = program})
                print result
                loop machine''

lisp = run . compile . parse

example1 = SInt 42
example2 = SExpr [SSymbol "+", SInt 7, SInt 3]
example3 = SExpr [SSymbol "+", SInt 7, SExpr [SSymbol "-", SInt 6, SInt 2]]
example4 = SExpr [SSymbol "+", SInt 4, SInt 3, SInt 2, SInt 1]

compile = concat . intersperse " " . optimise . concat . map compileTokens

compileTokens (SInt x)       = [show x]
compileTokens (SQuote expr)  = compileQuoted expr
compileTokens (SExpr [(SSymbol "define"), SSymbol name, body]) = compileTokens body ++ [":" ++ name, "def"]
compileTokens (SExpr [(SSymbol "lambda"), SExpr vars, body]) = block (reverse [":" ++ x ++ " def" | SSymbol x <- vars] ++ compileTokens body)
compileTokens (SExpr [(SSymbol "if"), cond, true_branch, false_branch]) = compileTokens cond ++ block(compileTokens true_branch) ++ block(compileTokens false_branch) ++ ["if"]
compileTokens (SExpr (x:xs)) = concat (map compileTokens xs) ++ compileTokens x ++ ["call"]
compileTokens (SExpr [])     = [] 
compileTokens (SSymbol x)    = [":" ++ x, "!"]

compileQuoted (SExpr [])     = ["nil"]
compileQuoted (SExpr (x:xs)) = compileQuoted (SExpr xs) ++ compileQuoted x ++ [","]
compileQuoted (SSymbol x)    = [":" ++ x]
compileQuoted (SInt x)       = [show x]

block instructions = ["["] ++ instructions ++ ["]"]

optimise (":=":"!":"call":xs) = "=":optimise xs
optimise (":<":"!":"call":xs) = "<":optimise xs
optimise (":>":"!":"call":xs) = ">":optimise xs
optimise (":-":"!":"call":xs) = "-":optimise xs
optimise (":+":"!":"call":xs) = "+":optimise xs
optimise (":*":"!":"call":xs) = "*":optimise xs
optimise (":/":"!":"call":xs) = "-":optimise xs
optimise (x:xs) = x:optimise xs
optimise [] = []

-- Parser - probably separate.
parse = fst . parse' . splitTokens

parse' :: [String] -> ([SExpr], [String])
parse' []       = ([], [])
parse' ("'":xs) = (SQuote y:ys, rest)
    where ((y:ys), rest) = parse' xs
parse' ("(":xs) = (SExpr this:that, rest')
    where (this, rest)  = parse' xs
          (that, rest') = parse' rest
parse' (")":xs) = ([], xs)
parse' (x:xs) = (parseSymbol x:that, rest)
    where (that, rest) = parse' xs          

parseSymbol x | isNumber x = SInt (read x)
              | otherwise       = SSymbol x

isNumber = all isDigit
isDigit  x = x >= '0' && x <= '9'
isSymbol x = x >= '*' && x <= '?' || x >= '^' && x <= 'z'

splitTokens :: String -> [String]
splitTokens "" = []
splitTokens ('\'':xs) = "'" : splitTokens xs
splitTokens ('(':xs) = "(" : splitTokens xs
splitTokens (')':xs) = ")" : splitTokens xs
splitTokens (' ':xs) = splitTokens xs
splitTokens ('\n':xs) = splitTokens xs
splitTokens ('\t':xs) = splitTokens xs
splitTokens (x:xs) | isSymbol x = let (token, tail) = break (not.isSymbol) xs in (x:token) : splitTokens tail
