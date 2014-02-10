module SExprs where

data SExpr = SList [SExpr] | SSymbol String | SInt Integer | SBool Bool | SString String
    deriving (Eq)
