module VmCode (parseProgram, formatProgram, readInstruction) where

import Data.List
import Control.Applicative ((<*), (*>), (<$>))
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Numbers

import VmTypes

parseProgram :: String -> Either ParseError [Symbol]
parseProgram = parse program "(unknown!)"

program = (many (space <|> comment) *> termList <* eof)

termList = endBy term (many (space <|> comment))

term  = try value <|> try instruction

value = Value <$> (number <|> symbol <|> stringLit <|> block <|> bool <|> nil)

instruction = choice [try (string k >> return v) | (k, v) <- instructionMap]

number = parseIntegral >>= return . I

symbol = char ':' >> many (noneOf " \t\n") >>= return . S 
stringLit = do
    char '"'
    str <- many ((noneOf "\"\\") <|> (char '\\' >> escapedChar))
    char '"'
    return $ Str str

escapedChar = do
    x <- anyChar
    return $ case x of
        'n'  -> '\n'
        't'  -> '\t'
        _    -> x

block = do
    char '['
    many space
    instructions <- termList
    char ']'
    return $ CP instructions

bool = (string "true" >> return (B True)) <|> (string "false" >> return (B False))
nil  = string "nil" >> return Nil

comment = char '#' >> many (noneOf "\n") >> (optional $ char '\n') >> return '\n'

formatProgram xs = concat $ intersperse " " $ map f xs
    where f (Value x) = formatVal x
          f ins = head $ [k | (k, v) <- instructionMap, v == ins]

readInstruction x = case [v | (k, v) <- instructionMap, k == x] of
                        [y] -> y;
                        []  -> error $ "Invalid VM instruction: " ++ x

formatVal (I x) = show x
formatVal (B True) = "true"
formatVal (B False) = "false"
formatVal Nil = "nil"
formatVal (S x) = ":" ++ x
formatVal (Str x) = show x
formatVal (CP xs) = "[ " ++ formatProgram xs ++ " ]"

instructionMap = [("+", Add),
               ("-", Sub),
               ("*", Mul),
               (">", Gt),
               (">=", Ge),
               ("<=", Le),
               ("<", Lt),
               ("=", Eq),
               ("dup", Dup),
               ("flip", Flip),
               ("store", Store),
               ("lookup", Lookup),
               ("rot", Rot),
               ("jump", Jmp),
               ("not", Not),
               ("if", If),
               ("drop", Drop),
               ("cons", Cons),
               ("decons", DeCons),
               ("save", SaveEnv),
               ("new", NewFrame),
               ("load", LoadEnv),
               ("show", Show),
               ("type?", Type),
               ("port", GetPort),
               ("write", Write),
               ("read", Read),
               ("error", Error)]
