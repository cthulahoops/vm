module VmCode (parseProgram) where

import Control.Applicative ((<*), (*>))
import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Numbers

import VmTypes

parseProgram :: String -> Either ParseError [Symbol]
parseProgram = parse program "(unknown!)"

program = (many (space <|> comment) *> termList <* eof)

termList = endBy term (many (space <|> comment))

term  = try instruction <|> try value

value = do
    val <- number <|> symbol <|> stringLit <|> block <|> bool <|> nil
    return $ Value val

instruction = choice [try (string k >> return (Instruction v)) | (k, v) <- instructionMap]

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
               ("def", Store),
               ("!", Lookup),
               ("rot", Rot),
               ("jmp", Jmp),
               ("not", Not),
               ("if", If),
               ("drop", Drop),
               (",", Cons),
               ("`", DeCons),
               ("&", SaveEnv),
               ("{}", NewFrame),
               ("$", LoadEnv),
               ("show", Show),
               ("?", Type),
               ("port", GetPort),
               ("w", Write),
               ("r", Read)]
