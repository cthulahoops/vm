module Compile where

import Control.Applicative
import Data.List
import VmTypes
import VmCode

import SExprs

type Tok = Symbol

compile = concat . intersperse "\n" . map formatProgram . map compileTokens

compileTokens :: SExpr -> [Tok]
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
compileExpr (SInt x)       = [Value $ I x]
compileExpr (SBool True)   = [Value $ B True]
compileExpr (SBool False)  = [Value $ B False]
compileExpr (SString str)  = [Value $ Str str]
compileExpr (SCons (SSymbol "quote") expr)  = compileQuoted expr
compileExpr (SCons (SSymbol "define") (SCons (SSymbol name) (SCons body SNil)))
                           = compileExpr body ++ [vmSymbol name, Store, Value Nil]
compileExpr (SCons (SSymbol "lambda") (SCons vars body)) = compileLambda vars body
compileExpr (SCons (SSymbol "if") (SCons cond (SCons true_branch (SCons false_branch SNil))))
                           = compileIf cond true_branch false_branch
compileExpr (SCons (SSymbol "$vm-op") (SCons (SInt arity) instructions)) = [Value Nil] ++ block ([Drop] ++ concat (replicate (fromIntegral arity) [DeCons, Flip]) ++ [Drop] ++ mapToList getSymbol instructions) ++ [Cons]
         where getSymbol (SSymbol x) = readInstruction x
compileExpr (SCons (SSymbol "begin") exprs) = concat $ intersperse [Drop] $ mapToList compileExpr exprs
compileExpr (SCons (SSymbol "apply") (SCons function (SCons args SNil))) = applyLambda (compileExpr function) (compileExpr args)
compileExpr (SCons f args)   = applyLambda (compileExpr f) (compileArgs args)
compileExpr (SSymbol x)    = [vmSymbol x, Lookup]

compileQuoted SNil     = [Value Nil]
compileQuoted (SCons car cdr) = compileQuoted cdr ++ compileQuoted car ++ [Cons]
compileQuoted (SSymbol x)    = [vmSymbol x]
compileQuoted (SInt x)       = [Value $ I x]
compileQuoted (SString x)    = [Value $ Str x]

compileIf cond true_branch false_branch = block(compileExpr true_branch) ++ block(compileExpr false_branch) ++ compileExpr cond ++ [If, Jmp]

compileLambda :: SExpr -> SExpr -> [Tok]
compileLambda params body = [SaveEnv] ++ block ([NewFrame, LoadEnv] ++ compileParams params ++ concat (intersperse [Drop] (mapToList compileExpr body))) ++ [Cons]
    where compileParams SNil                     = [Drop]
          compileParams (SSymbol x)              = [vmSymbol x, Store]
          compileParams (SCons (SSymbol x) rest) = [DeCons, vmSymbol x, Store] ++ compileParams rest

block instructions = [Value (CP instructions)]

compileArgs args = [Value Nil] ++ concat (reverse (mapToList (\x -> compileExpr x ++ [Cons]) args))

applyLambda :: [Tok] -> [Tok] -> [Tok]
applyLambda function args = [SaveEnv] ++ args ++ function ++ [DeCons, Jmp, Flip, LoadEnv]

vmSymbol x = Value $ S x

optimise = id
