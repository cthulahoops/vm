module Compile where

import Control.Applicative
import Data.List
import VmTypes

import SExprs

type Tok = String

compile = concat . intersperse " " . optimise . concat . map compileTokens

compileTokens = compileExpr . transform

transform :: SExpr -> SExpr
transform (SExpr ((SSymbol "let"):SExpr bindings:body)) = SExpr (SExpr (SSymbol "lambda":SExpr vars:body):values)
    where (vars, values) = unzip $ map toPair bindings
          toPair (SExpr [SSymbol x, expr]) = (SSymbol x, expr)
transform (SExpr (SSymbol "let*":SExpr [binding]:body)) = transform $ SExpr (SSymbol "let":SExpr [binding]:body)
transform (SExpr (SSymbol "let*":SExpr (binding:bindings):body)) = transform $ SExpr [SSymbol "let", SExpr [binding], SExpr (SSymbol "let*":SExpr bindings:body)]
transform (SExpr (SSymbol "define":SExpr (SSymbol name:vars): body))
    | all (\(SSymbol s) -> True) vars = SExpr [SSymbol "define", SSymbol name, SExpr ((SSymbol "lambda":SExpr vars:body))]
transform x = x

-- Function Call: "& 7 :f ! ` jmp flip $"
-- Function Def:  "& [ {} $ :n def 2 :n ! * ] , :f def"
compileExpr (SInt x)       = [show x]
compileExpr (SBool True)   = ["true"]
compileExpr (SBool False)  = ["false"]
compileExpr (SString str)  = [show str]
compileExpr (SExpr [SSymbol "quote", expr])  = compileQuoted expr
compileExpr (SExpr [SSymbol "define", SSymbol name, body]) = compileTokens body ++ [":" ++ name, "def", "nil"]
compileExpr (SExpr (SSymbol "lambda":vars:body)) = makeLambda vars body
compileExpr (SExpr [SSymbol "if", cond, true_branch, false_branch]) = makeIf cond true_branch false_branch
compileExpr (SExpr [SSymbol "apply", function, args]) = applyLambda function args
compileExpr (SExpr (x:xs)) = applyLambda x (SExpr xs)
compileExpr (SExpr [])     = [] 
compileExpr (SSymbol x)    = [":" ++ x, "!"]

compileQuoted (SExpr [])     = ["nil"]
compileQuoted (SExpr (x:xs)) = compileQuoted (SExpr xs) ++ compileQuoted x ++ [","]
compileQuoted (SSymbol x)    = [":" ++ x]
compileQuoted (SInt x)       = [show x]

makeIf cond true_branch false_branch = block(compileTokens true_branch) ++ block(compileTokens false_branch) ++ compileTokens cond ++ ["if", "jmp"]

makeLambda (SExpr vars)  body = ["&"] ++ block (["{}", "$"] ++ concat [["`", ":" ++ x, "def"] | SSymbol x <- vars] ++ ["drop"] ++ concat (intersperse ["drop"] (map compileTokens body))) ++ [","]
makeLambda (SSymbol x) body = ["&"] ++ block (["{}", "$"] ++ [":" ++ x, "def"] ++ concat (intersperse ["drop"] (map compileTokens body))) ++ [","]

block instructions = ["["] ++ instructions ++ ["]"]

applyLambda :: SExpr -> SExpr -> [Tok]
applyLambda function (SExpr args) = ["&"] ++ compiledArgs ++ compileTokens function ++ ["`", "jmp", "flip", "$"]
    where compiledArgs = ["nil"] ++ concat (map (\x -> compileTokens x ++ [","]) (reverse args))

optimise = id
