module Compile where

import Control.Applicative
import Data.List
import Assemble
import Vm

data SExpr = SExpr [SExpr] | SSymbol String | SInt Integer | SQuote SExpr
    deriving Show

lisp = run . compile . parse

compile = concat . intersperse " " . optimise . concat . map compileTokens

compileTokens = compileExpr . transform

transform :: SExpr -> SExpr
transform (SExpr ((SSymbol "let"):SExpr bindings:body)) = SExpr (SExpr (SSymbol "lambda":SExpr vars:body):values)
    where (vars, values) = unzip $ map toPair bindings
          toPair (SExpr [SSymbol x, expr]) = (SSymbol x, expr)
transform (SExpr (SSymbol "define":SExpr (SSymbol name:vars): body))
    | all (\(SSymbol s) -> True) vars = SExpr [SSymbol "define", SSymbol name, SExpr ((SSymbol "lambda":SExpr vars:body))]
transform x = x

-- Function Call: "& 7 :f ! ` jmp flip $"
-- Function Def:  "& [ {} $ :n def 2 :n ! * ] , :f def"
compileExpr (SInt x)       = [show x]
compileExpr (SQuote expr)  = compileQuoted expr
compileExpr (SExpr [SSymbol "define", SSymbol name, body]) = compileTokens body ++ [":" ++ name, "def", "nil"]
compileExpr (SExpr (SSymbol "lambda":SExpr vars:body)) = ["&"] ++ block ( ["{}", "$"] ++ reverse [":" ++ x ++ " def" | SSymbol x <- vars] ++ concat (intersperse ["drop"] (map compileTokens body))) ++ [","]
compileExpr (SExpr [(SSymbol "if"), cond, true_branch, false_branch]) = block(compileTokens true_branch) ++ block(compileTokens false_branch) ++ compileTokens cond ++ ["if", "jmp"]
compileExpr (SExpr (x:xs)) = ["&"] ++ concat (map compileTokens xs) ++ compileTokens x ++ ["`", "jmp", "flip", "$"]
compileExpr (SExpr [])     = [] 
compileExpr (SSymbol x)    = [":" ++ x, "!"]

compileQuoted (SExpr [])     = ["nil"]
compileQuoted (SExpr (x:xs)) = compileQuoted (SExpr xs) ++ compileQuoted x ++ [","]
compileQuoted (SSymbol x)    = [":" ++ x]
compileQuoted (SInt x)       = [show x]

block instructions = ["["] ++ instructions ++ ["]"]

optimise = id
-- optimise (x:xs) = x:optimise xs
-- optimise [] = []

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
