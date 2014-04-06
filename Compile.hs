module Compile where

import Control.Applicative
import Data.List
import VmTypes

import SExprs

type Tok = String

compile = concat . intersperse " " . optimise . concat . map compileTokens

compileTokens = compileExpr . transform

transform :: SExpr -> SExpr
transform x@(SCons (SSymbol "quote") xs) = x
transform x@(SCons car cdr) = if transformed == x
                            then mapCars transform transformed
                            else transform transformed
            where transformed = transform' car cdr
transform x = x

mapToList f = map f . toList

transform' :: SExpr -> SExpr -> SExpr
transform' (SSymbol "cond") expr = condToIf expr
transform' (SSymbol "let")  expr = letToLambda expr
transform' (SSymbol "let*") expr = letStarToLet expr
transform' (SSymbol "define") (SCons (SCons (SSymbol name) vars) body) = 
    fromList $ [SSymbol "define", SSymbol name, makeLambda vars body]
-- transform' (SSymbol "define":SList (SSymbol name:vars): body)
--     | all (\(SSymbol s) -> True) vars = SList $ [SSymbol "define", SSymbol name, SList ((SSymbol "lambda":SList vars:body))]
transform' car cdr = SCons car cdr

condToIf SNil = SNil
condToIf (SCons (SCons (SSymbol "else") (SCons body SNil)) SNil)  = body
condToIf (SCons (SCons cond (SCons body SNil)) more) = makeIf cond body (condToIf more)
     where makeIf cond_ then_ else_ = fromList $ [SSymbol "if", cond_, then_, else_]

letToLambda (SCons bindings body) = fromList $ (makeLambda (fromList vars) body):values
     where (vars, values) = unzip $ mapToList toPair bindings
           toPair (SCons (SSymbol x) (SCons expr SNil)) = (SSymbol x, expr)

letStarToLet (SCons SNil body) = (SCons (SSymbol "begin") body)
letStarToLet (SCons (SCons binding bindings) body) = fromList [SSymbol "let", fromList [binding], fromList (SSymbol "let*":bindings:toList body)]

makeLambda vars body = fromList $ (SSymbol "lambda":vars:toList body)

-- Function Call: "& 7 :f ! ` jmp flip $"
-- Function Def:  "& [ {} $ :n def 2 :n ! * ] , :f def"
compileExpr (SInt x)       = [show x]
compileExpr (SBool True)   = ["true"]
compileExpr (SBool False)  = ["false"]
compileExpr (SString str)  = [show str]
compileExpr (SCons (SSymbol "quote") expr)  = compileQuoted expr
compileExpr (SCons (SSymbol "define") (SCons (SSymbol name) (SCons body SNil)))
                           = compileExpr body ++ [vmSymbol name, "def", "nil"]
compileExpr (SCons (SSymbol "lambda") (SCons vars body)) = compileLambda vars body
compileExpr (SCons (SSymbol "if") (SCons cond (SCons true_branch (SCons false_branch SNil))))
                           = compileIf cond true_branch false_branch
compileExpr (SCons (SSymbol "$vm-op") (SCons (SInt arity) instructions)) = ["nil"] ++ block (["drop"] ++ concat (replicate (fromIntegral arity) ["`", "flip"]) ++ ["drop"] ++ mapToList getSymbol instructions) ++ [","]
        where getSymbol (SSymbol x) = x
compileExpr (SCons (SSymbol "begin") exprs) = concat $ intersperse ["drop"] $ mapToList compileExpr exprs
compileExpr (SCons (SSymbol "apply") (SCons function (SCons args SNil))) = applyLambda (compileExpr function) (compileExpr args)
compileExpr (SCons f args)   = applyLambda (compileExpr f) (compileArgs args)
compileExpr (SSymbol x)    = [vmSymbol x, "!"]

compileQuoted SNil     = ["nil"]
compileQuoted (SCons car cdr) = compileQuoted cdr ++ compileQuoted car ++ [","]
compileQuoted (SSymbol x)    = [vmSymbol x]
compileQuoted (SInt x)       = [show x]
compileQuoted (SString x)    = [show x]

compileIf cond true_branch false_branch = block(compileExpr true_branch) ++ block(compileExpr false_branch) ++ compileExpr cond ++ ["if", "jmp"]

compileLambda :: SExpr -> SExpr -> [Tok]
compileLambda params body = ["&"] ++ block (["{}", "$"] ++ compileParams params ++ concat (intersperse ["drop"] (mapToList compileExpr body))) ++ [","]
    where compileParams SNil                     = ["drop"]
          compileParams (SSymbol x)              = [vmSymbol x, "def"]
          compileParams (SCons (SSymbol x) rest) = ["`", vmSymbol x, "def"] ++ compileParams rest

block instructions = ["["] ++ instructions ++ ["]"]

compileArgs args = ["nil"] ++ concat (reverse (mapToList (\x -> compileExpr x ++ [","]) args))

applyLambda :: [Tok] -> [Tok] -> [Tok]
applyLambda function args = ["&"] ++ args ++ function ++ ["`", "jmp", "flip", "$"]

vmSymbol x = ":" ++ x

optimise = id
