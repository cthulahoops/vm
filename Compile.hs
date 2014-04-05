module Compile where

import Control.Applicative
import Data.List
import VmTypes

import SExprs

type Tok = String

compile = concat . intersperse " " . optimise . concat . map compileTokens

compileTokens = compileExpr . transform

transform :: SExpr -> SExpr
transform x@(SList (SSymbol "quote":xs)) = x
transform (SList ss) = if transformed == SList ss
                       then SList (map transform ss)
                       else transform transformed
            where transformed = transform' ss
transform x = x

transform' :: [SExpr] -> SExpr
transform' [SSymbol "cond"] = SList []
transform' [SSymbol "cond", SList [SSymbol "else", body]] = body
transform' ((SSymbol "cond"):SList [cond, body]:more) = SList [SSymbol "if", cond, body, f more]
    where f _  = SList (SSymbol "cond":more)
transform' ((SSymbol "let"):SList bindings:body) = SList (SList (SSymbol "lambda":SList vars:body):values)
    where (vars, values) = unzip $ map toPair bindings
          toPair (SList [SSymbol x, expr]) = (SSymbol x, expr)
transform' (SSymbol "let*":SList [binding]:body) = SList $ (SSymbol "let":SList [binding]:body)
transform' (SSymbol "let*":SList (binding:bindings):body) = SList $ [SSymbol "let", SList [binding], SList (SSymbol "let*":SList bindings:body)]
transform' (SSymbol "define":SList (SSymbol name:vars): body)
    | all (\(SSymbol s) -> True) vars = SList $ [SSymbol "define", SSymbol name, SList ((SSymbol "lambda":SList vars:body))]
transform' xs = SList xs

-- Function Call: "& 7 :f ! ` jmp flip $"
-- Function Def:  "& [ {} $ :n def 2 :n ! * ] , :f def"
compileExpr (SInt x)       = [show x]
compileExpr (SBool True)   = ["true"]
compileExpr (SBool False)  = ["false"]
compileExpr (SString str)  = [show str]
compileExpr (SList [SSymbol "quote", expr])  = compileQuoted expr
compileExpr (SList [SSymbol "define", SSymbol name, body]) = compileExpr body ++ [":" ++ name, "def", "nil"]
compileExpr (SList (SSymbol "lambda":vars:body)) = makeLambda vars body
compileExpr (SList [SSymbol "if", cond, true_branch, false_branch]) = makeIf cond true_branch false_branch
compileExpr (SList (SSymbol "$vm-op":SInt arity:instructions)) = ["nil"] ++ block (["drop"] ++ concat (replicate (fromIntegral arity) ["`", "flip"]) ++ ["drop"] ++ [x | SSymbol x <- instructions]) ++ [","]
compileExpr (SList (SSymbol "begin":exprs)) = concat $ intersperse ["drop"] $ map compileExpr exprs
compileExpr (SList [SSymbol "apply", function, args]) = applyLambda (compileExpr function) (compileExpr args)
compileExpr (SList (x:xs)) = applyLambda (compileExpr x) (compileArgs xs)
compileExpr (SList [])     = [] 
compileExpr (SSymbol x)    = [":" ++ x, "!"]

compileQuoted (SList [])     = ["nil"]
compileQuoted (SList (x:xs)) = compileQuoted (SList xs) ++ compileQuoted x ++ [","]
compileQuoted (SSymbol x)    = [":" ++ x]
compileQuoted (SInt x)       = [show x]
compileQuoted (SString x)    = [show x]

makeIf cond true_branch false_branch = block(compileExpr true_branch) ++ block(compileExpr false_branch) ++ compileExpr cond ++ ["if", "jmp"]

makeLambda (SList vars)  body = ["&"] ++ block (["{}", "$"] ++ concat [["`", ":" ++ x, "def"] | SSymbol x <- vars] ++ ["drop"] ++ concat (intersperse ["drop"] (map compileExpr body))) ++ [","]
makeLambda (SSymbol x) body = ["&"] ++ block (["{}", "$"] ++ [":" ++ x, "def"] ++ concat (intersperse ["drop"] (map compileExpr body))) ++ [","]

block instructions = ["["] ++ instructions ++ ["]"]

compileArgs args = ["nil"] ++ concat (map (\x -> compileExpr x ++ [","]) (reverse args))

applyLambda :: [Tok] -> [Tok] -> [Tok]
applyLambda function args = ["&"] ++ args ++ function ++ ["`", "jmp", "flip", "$"]

optimise = id
