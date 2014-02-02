module SExprs where

data SExpr = SExpr [SExpr] | SSymbol String | SInt Integer | SBool Bool | SString String
    deriving Show
