module Compile where

import Control.Applicative
import Data.List
import VmTypes
import VmCode

import SExprs

compile :: [SExpr] -> [Symbol]
compile = optimise . compileSequence False . map transform

transform :: SExpr -> SExpr
transform x@(SCons (SValue (SSymbol "quote")) xs) = x
transform x@(SCons car cdr) = if transformed == x
                            then mapCars transform transformed
                            else transform transformed
            where transformed = transform' car cdr
transform x = x

mapToList f = map f . toList

transform' :: SExpr -> SExpr -> SExpr 
transform' car cdr | isSymbol "cond" car   = condToIf cdr
                   | isSymbol "let" car    = letToLambda cdr
                   | isSymbol "let*" car   = letStarToLet cdr
                   | isSymbol "define" car = transformDefine cdr
                   | isSymbol "and" car    = andToIf cdr
                   | isSymbol "or" car     = orToIf cdr
                   | otherwise             = SCons car cdr

transformDefine (SCons (SCons (SValue (SSymbol name)) vars) body) =
    fromList $ [SValue (SSymbol "define"), SValue (SSymbol name), makeLambda vars body]
transformDefine other = (SCons (symbol "define") other)

condToIf SNil = SNil
condToIf (SCons (SCons (SValue (SSymbol "else")) body) SNil)  = (SCons (SValue (SSymbol "begin")) body)
condToIf (SCons (SCons cond body) more) = makeIf cond (SCons (SValue (SSymbol "begin")) body) (condToIf more)

letToLambda (SCons bindings body) = fromList $ (makeLambda (fromList vars) body):values
     where (vars, values) = unzip $ mapToList toPair bindings
           toPair (SCons (SValue (SSymbol x)) (SCons expr SNil)) = (SValue $ SSymbol x, expr)

letStarToLet (SCons SNil body) = (SCons (SValue (SSymbol "begin")) body)
letStarToLet (SCons (SCons binding bindings) body) = fromList [SValue (SSymbol "let"), fromList [binding], fromList (SValue (SSymbol "let*"):bindings:toList body)]


andToIf SNil = strue
andToIf (SCons expr SNil) = fromList [symbol "let",
    fromList [fromList [symbol "$and-var", expr]],
    makeIf (symbol "$and-var") (symbol "$and-var") sfalse]
andToIf (SCons expr exprs) = makeIf expr (SCons (symbol "and") exprs) sfalse

orToIf SNil = sfalse
orToIf (SCons expr exprs) = fromList [symbol "let",
    fromList [fromList [symbol "$or-var", expr]],
    makeIf (symbol "$or-var") (symbol "$or-var") (SCons (symbol "or") exprs)]

makeLambda vars body = fromList $ (symbol "lambda":vars:toList body)
makeIf cond_ then_ else_ = fromList $ [symbol "if", cond_, then_, else_]

scar (SCons car cdr) = car
scdr (SCons car cdr) = cdr

unpack3 (SCons a (SCons b (SCons c SNil))) = (a, b, c)
unpack2 (SCons a (SCons b SNil)) = (a, b)

-- Function Call: "& 7 :f ! ` jmp flip $"
-- Function Def:  "& [ {} $ :n def 2 :n ! * ] , :f def"
compileExpr :: Bool -> SExpr -> [Symbol]
compileExpr isTail (SCons car cdr) 
    | isSymbol "quote" car  = compileQuoted cdr
    | isSymbol "define" car = compileDefine cdr
    | isSymbol "lambda" car = compileLambda (scar cdr) (scdr cdr)
    | isSymbol "if" car     = let (cond, trueBranch, falseBranch) = unpack3 cdr
                                in compileIf isTail cond trueBranch falseBranch
    | isSymbol "$vm-op" car = compileVmOp (scar cdr) (scdr cdr)
    | isSymbol "begin" car  = compileSequence isTail $ toList cdr
    | isSymbol "apply" car  = let (function, args) = unpack2 cdr
                                in applyLambda isTail (compileExpr False function) (compileExpr False args)
    | otherwise             = applyLambda isTail (compileExpr False car) (compileArgs cdr)
compileExpr isTail (SValue (SSymbol x)) = [Push $ S x, Lookup]
compileExpr isTail (SValue x)  = [Push $ compileValue x]

compileValue :: SValue -> Val
compileValue (SInt x)    = I x
compileValue (SBool x)   = B x
compileValue (SString x) = Str x
compileValue (SSymbol x) = S x

compileQuoted SNil            = [Push Nil]
compileQuoted (SCons car cdr) = compileQuoted cdr ++ compileQuoted car ++ [Cons]
compileQuoted (SValue val)    = [Push $ compileValue val]

compileIf :: Bool -> SExpr -> SExpr -> SExpr -> [Symbol]
compileIf isTail cond trueBranch falseBranch = block(compileExpr True trueBranch)
                                            ++ block(compileExpr True falseBranch)
                                            ++ compileExpr False cond ++ [Push (B False), Eq, Not]
                                            ++ [If, (if isTail then Jump else Call)]

compileLambda :: SExpr -> SExpr -> [Symbol]
compileLambda params body = [SaveEnv]
                         ++ block ([NewFrame, LoadEnv]
                                 ++ compileParams params
                                 ++ compileSequence True (toList body))
                         ++ [Cons]
    where compileBody body = map (compileExpr False) (init body) ++ [compileExpr True (last body)]
          compileParams SNil                     = [Drop]
          compileParams (SValue (SSymbol x))     = [Push $ S x, Store]
          compileParams (SCons (SValue (SSymbol x)) rest) = [DeCons, Push $ S x, Store] ++ compileParams rest

compileDefine (SCons (SValue (SSymbol name)) (SCons body SNil)) =
    compileExpr False body ++ [Push $ S name, Store, Push Nil]

compileVmOp (SValue (SInt arity)) ins = [Push Nil] ++ block functionBody ++ [Cons]
         where functionBody = [Drop]
                           ++ concat (replicate (fromIntegral arity) [DeCons, Flip])
                           ++ [Drop]
                           ++ mapToList getSymbol ins
               getSymbol (SValue (SSymbol "nil")) = Push Nil
               getSymbol (SValue (SSymbol x))     = readInstruction x
               getSymbol a@(SCons _ _)   = head $ block $ map getSymbol $ toList a

compileSequence :: Bool -> [SExpr] -> [Symbol]
compileSequence isTail seq = concat
                          $ intersperse [Drop]
                          $ map (compileExpr False) (init seq) ++ [compileExpr isTail (last seq)]

block instructions = [Push (CP instructions)]

compileArgs args = [Push Nil] ++ concat (reverse (mapToList (\x -> compileExpr False x ++ [Cons]) args))

-- Calling a function:
--  Example: [SaveEnv,Push Nil,Push (I 7),Cons,Push (S "f"),Lookup,DeCons,Call,Flip,LoadEnv] 
--  SaveEnv - push callers environment onto the stack
--  Push Nil, Push (I 7), Cons - Push function arguments onto the stack.
--  Push (S "f"), Lookup - Get function to call onto the stack.
--  Decons - Unpack function environment and code.
--  Call - Push next instruction to return stack and jump to code.
--  On return, stack will consist of [Return Push, Caller Env]
--  Flip - Swap environment and return value.
--  LoadEnv - Restore the environment.
applyLambda :: Bool -> [Symbol] -> [Symbol] -> [Symbol]
applyLambda False function args = callReturn $ args ++ function
applyLambda True  function args = callTail   $ args ++ function

callTail   code = code ++ [DeCons, Jump]
callReturn code = [SaveEnv] ++ code ++ [DeCons, Call, Flip, LoadEnv]

optimise = optimisePushDrop . unusedFrame

optimisePushDrop [] = []
optimisePushDrop (Push _:Drop:code) = optimisePushDrop code
optimisePushDrop (Push (CP block):code) = Push (CP (optimisePushDrop block)):optimisePushDrop code
optimisePushDrop (x:code) = x:optimisePushDrop code

unusedFrame (NewFrame:LoadEnv:code) = (if usesStore code then [NewFrame,LoadEnv] else [LoadEnv]) ++ unusedFrame code
unusedFrame (Push (CP block):code) = Push (CP (unusedFrame block)):unusedFrame code
unusedFrame (x:code) = x:unusedFrame code
unusedFrame [] = []

usesStore (Store:code) = True
usesStore (NewFrame:LoadEnv:code) = False
usesStore (Push (CP block):code) = usesStore block || usesStore code
usesStore (x:code) = usesStore code
usesStore [] = False

