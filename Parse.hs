module Parse where

import Data.Functor
import Control.Applicative ((<*), (*>))
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Numbers

import SExprs

parseExprs :: String -> Either ParseError [SExpr]
parseExprs = parse (sexprList <* eof) "(unknown!)"

sexpr = parens <|> (try int) <|> bool <|> symbol <|> stringLit <|> quote

sexprList = many space >> endBy sexpr (many space)

quote = do
    char '\''
    quoted <- sexpr
    return $ SList [SSymbol "quote", quoted] 

parens = SList <$> ((char '(' *> sexprList  <* char ')') <|> (char '[' *> sexprList  <* char ']'))

symbol = many1 (noneOf reserved) >>= return . SSymbol
int = do
    n <- parseIntegral
    lookAhead (oneOf reserved)
    return $ SInt n

bool = char '#' >> (char 't' *> return (SBool True) <|> char 'f' *> return (SBool False))
stringLit = do
    char '"'
    str <- many ((noneOf "\"\\") <|> (char '\\' >> anyChar))
    char '"'
    return $ SString str

reserved = " \t\n()[]\"\'"
