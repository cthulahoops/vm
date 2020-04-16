{-# LANGUAGE FlexibleContexts #-}
module Parse (parseExprs) where

import Data.Functor
import Control.Applicative ((<*), (*>))
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Numbers

import SExprs hiding (symbol)

parseExprs :: String -> Either ParseError [SExpr]
parseExprs = parse (sexprList <* eof) "(unknown!)"

sexpr = parens <|> (SValue <$> ((try int) <|> bool <|> symbol <|> stringLit)) <|> quote

sexprList = many space >> endBy sexpr (many space)

quote = do
     char '\''
     quoted <- sexpr
     return $ SCons (SValue $ SSymbol "quote") quoted

-- parens = SList <$> ((char '(' *> sexprList  <* char ')') <|> (char '[' *> sexprList  <* char ']'))

parens = (char '(' >> sexprTail ')') <|> (char '[' >> sexprTail ']')

sexprTail close = many space >> (dottedTail close <|> listTail close <|> listEnd close)

dottedTail close = do
    char '.'
    many space
    tail <- sexpr
    char close
    return tail

listTail close = do
    car <- sexpr
    cdr <- sexprTail close
    return $ SCons car cdr

listEnd close = char close >> return SNil

symbol = many1 (noneOf reserved) >>= return . SSymbol
int = do
    n <- parseIntegral
    lookAhead (oneOf reserved)
    return $ SInt n

bool = char '#' >> (char 't' *> return (SBool True) <|> char 'f' *> return (SBool False))
stringLit = do
    char '"'
    str <- many $ (noneOf "\"\\") <|> (char '\\' >> (control <|> anyChar))
    char '"'
    return $ SString str

control = (char 'n' >> return '\n') <|> (char 't' >> return '\t')

reserved = " \t\n()[]\"\'"
