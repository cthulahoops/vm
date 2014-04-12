module Compile where

import Control.Applicative
import Data.List
import VmTypes
import VmCode

import SExprs

compile = concat . intersperse "\n" . map formatProgram . map compileTokens

compileTokens :: SExpr -> [Symbol]
compileTokens = optimise . compileExpr False . transform

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
compileExpr :: Bool -> SExpr -> [Symbol]
compileExpr isTail (SInt x)       = [Value $ I x]
compileExpr isTail (SBool True)   = [Value $ B True]
compileExpr isTail (SBool False)  = [Value $ B False]
compileExpr isTail (SString str)  = [Value $ Str str]
compileExpr isTail (SCons (SSymbol "quote") expr)  = compileQuoted expr
compileExpr isTail (SCons (SSymbol "define") (SCons (SSymbol name) (SCons body SNil)))
                           = compileExpr False body ++ [vmSymbol name, Store, Value Nil]
compileExpr isTail (SCons (SSymbol "lambda") (SCons vars body)) = compileLambda vars body
compileExpr isTail (SCons (SSymbol "if") (SCons cond (SCons true_branch (SCons false_branch SNil))))
                           = compileIf isTail cond true_branch false_branch
compileExpr isTail (SCons (SSymbol "$vm-op") (SCons (SInt arity) ins)) = compileVmOp arity ins
compileExpr isTail (SCons (SSymbol "begin") exprs) = compileSeqence isTail (toList exprs)
compileExpr isTail (SCons (SSymbol "apply") (SCons function (SCons args SNil))) = applyLambda isTail (compileExpr False function) (compileExpr False args)
compileExpr isTail (SCons f args)   = applyLambda isTail (compileExpr False f) (compileArgs args)
compileExpr isTail (SSymbol x)    = [vmSymbol x, Lookup]

compileQuoted SNil     = [Value Nil]
compileQuoted (SCons car cdr) = compileQuoted cdr ++ compileQuoted car ++ [Cons]
compileQuoted (SSymbol x)    = [vmSymbol x]
compileQuoted (SInt x)       = [Value $ I x]
compileQuoted (SString x)    = [Value $ Str x]

compileIf isTail cond true_branch false_branch = block(compileExpr True true_branch)
                                              ++ block(compileExpr True false_branch)
                                              ++ compileExpr False cond ++ [Value (B False), Eq, Not]
                                              ++ [If, (if isTail then Jump else Call)]

compileLambda :: SExpr -> SExpr -> [Symbol]
compileLambda params body = [SaveEnv]
                         ++ block ([NewFrame, LoadEnv]
                                 ++ compileParams params
                                 ++ compileSeqence True (toList body))
                         ++ [Cons]
    where compileBody body = map (compileExpr False) (init body) ++ [compileExpr True (last body)]
          compileParams SNil                     = [Drop]
          compileParams (SSymbol x)              = [vmSymbol x, Store]
          compileParams (SCons (SSymbol x) rest) = [DeCons, vmSymbol x, Store] ++ compileParams rest

compileVmOp arity ins = [Value Nil] ++ block functionBody ++ [Cons]
         where functionBody = [Drop]
                           ++ concat (replicate (fromIntegral arity) [DeCons, Flip])
                           ++ [Drop]
                           ++ mapToList getSymbol ins
               getSymbol (SSymbol x) = readInstruction x

compileSeqence :: Bool -> [SExpr] -> [Ins]
compileSeqence isTail seq = concat
                          $ intersperse [Drop]
                          $ map (compileExpr False) (init seq) ++ [compileExpr isTail (last seq)]

block instructions = [Value (CP instructions)]

compileArgs args = [Value Nil] ++ concat (reverse (mapToList (\x -> compileExpr False x ++ [Cons]) args))

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
applyLambda :: Bool -> [Symbol] -> [Symbol] -> [Symbol]
applyLambda False function args = callReturn $ args ++ function
applyLambda True  function args = callTail   $ args ++ function

callTail   code = code ++ [DeCons, Jump]
callReturn code = [SaveEnv] ++ code ++ [DeCons, Call, Flip, LoadEnv]

vmSymbol x = Value $ S x

optimise = optimiseValueDrop . unusedFrame

optimiseValueDrop [] = []
optimiseValueDrop (Value _:Drop:code) = optimiseValueDrop code
optimiseValueDrop (Value (CP block):code) = Value (CP (optimiseValueDrop block)):optimiseValueDrop code
optimiseValueDrop (x:code) = x:optimiseValueDrop code

unusedFrame (NewFrame:LoadEnv:code) = (if usesStore code then [NewFrame,LoadEnv] else [LoadEnv]) ++ unusedFrame code
unusedFrame (Value (CP block):code) = Value (CP (unusedFrame block)):unusedFrame code
unusedFrame (x:code) = x:unusedFrame code
unusedFrame [] = []

usesStore (Store:code) = True
usesStore (NewFrame:LoadEnv:code) = False
usesStore (Value (CP block):code) = usesStore block || usesStore code
usesStore (x:code) = usesStore code
usesStore [] = False

