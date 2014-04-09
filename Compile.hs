module Compile where

import Control.Applicative
import Data.List
import VmTypes
import VmCode

import SExprs

compile = concat . intersperse "\n" . map formatProgram . map compileTokens

compileTokens :: SExpr -> [Symbol]
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
transform' (SSymbol "and") exprs = andToIf exprs
transform' (SSymbol "or") exprs = orToIf exprs
transform' car cdr = SCons car cdr

condToIf SNil = SNil
condToIf (SCons (SCons (SSymbol "else") (SCons body SNil)) SNil)  = body
condToIf (SCons (SCons cond (SCons body SNil)) more) = makeIf cond body (condToIf more)

letToLambda (SCons bindings body) = fromList $ (makeLambda (fromList vars) body):values
     where (vars, values) = unzip $ mapToList toPair bindings
           toPair (SCons (SSymbol x) (SCons expr SNil)) = (SSymbol x, expr)

letStarToLet (SCons SNil body) = (SCons (SSymbol "begin") body)
letStarToLet (SCons (SCons binding bindings) body) = fromList [SSymbol "let", fromList [binding], fromList (SSymbol "let*":bindings:toList body)]

andToIf SNil = SBool True
andToIf (SCons expr SNil) = fromList [SSymbol "let",
    fromList [fromList [SSymbol "$and-var", expr]],
    makeIf (SSymbol "$and-var") (SSymbol "$and-var") (SBool False)]
andToIf (SCons expr exprs) = makeIf expr (SCons (SSymbol "and") exprs) (SBool False)

orToIf SNil = SBool False
orToIf (SCons expr exprs) = fromList [SSymbol "let",
    fromList [fromList [SSymbol "$or-var", expr]],
    makeIf (SSymbol "$or-var") (SSymbol "$or-var") (SCons (SSymbol "or") exprs)]

makeLambda vars body = fromList $ (SSymbol "lambda":vars:toList body)
makeIf cond_ then_ else_ = fromList $ [SSymbol "if", cond_, then_, else_]

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
compileExpr (SCons (SSymbol "$vm-op") (SCons (SInt arity) ins)) = compileVmOp arity ins
compileExpr (SCons (SSymbol "begin") exprs) = concat $ intersperse [Drop] $ mapToList compileExpr exprs
compileExpr (SCons (SSymbol "apply") (SCons function (SCons args SNil))) = applyLambda (compileExpr function) (compileExpr args)
compileExpr (SCons f args)   = applyLambda (compileExpr f) (compileArgs args)
compileExpr (SSymbol x)    = [vmSymbol x, Lookup]

compileQuoted SNil     = [Value Nil]
compileQuoted (SCons car cdr) = compileQuoted cdr ++ compileQuoted car ++ [Cons]
compileQuoted (SSymbol x)    = [vmSymbol x]
compileQuoted (SInt x)       = [Value $ I x]
compileQuoted (SString x)    = [Value $ Str x]

compileIf cond true_branch false_branch = block(compileExpr true_branch)
                                       ++ block(compileExpr false_branch)
                                       ++ compileExpr cond ++ [Value (B False), Eq, Not]
                                       ++ [If, Call]

compileLambda :: SExpr -> SExpr -> [Symbol]
compileLambda params body = [SaveEnv] ++ block ([NewFrame, LoadEnv] ++ compileParams params ++ concat (intersperse [Drop] (mapToList compileExpr body))) ++ [Cons]
    where compileParams SNil                     = [Drop]
          compileParams (SSymbol x)              = [vmSymbol x, Store]
          compileParams (SCons (SSymbol x) rest) = [DeCons, vmSymbol x, Store] ++ compileParams rest

compileVmOp arity ins = [Value Nil] ++ block functionBody ++ [Cons]
         where functionBody = [Drop]
                           ++ concat (replicate (fromIntegral arity) [DeCons, Flip])
                           ++ [Drop]
                           ++ mapToList getSymbol ins
               getSymbol (SSymbol x) = readInstruction x

block instructions = [Value (CP instructions)]

compileArgs args = [Value Nil] ++ concat (reverse (mapToList (\x -> compileExpr x ++ [Cons]) args))

-- Calling a function:
--  Example: [SaveEnv,Value Nil,Value (I 7),Cons,Value (S "f"),Lookup,DeCons,Call,Flip,LoadEnv] 
--  SaveEnv - push callers environment onto the stack
--  Value Nil, Value (I 7), Cons - Push function arguments onto the stack.
--  Value (S "f"), Lookup - Get function to call onto the stack.
--  Decons - Unpack function environment and code.
--  Call - Push next instruction to return stack and jump to code.
--  On return, stack will consist of [Return Value, Caller Env]
--  Flip - Swap environment and return value.
--  LoadEnv - Restore the environment.
applyLambda :: [Symbol] -> [Symbol] -> [Symbol]
applyLambda function args = [SaveEnv] ++ args ++ function ++ [DeCons, Call, Flip, LoadEnv]

vmSymbol x = Value $ S x

optimise = id
