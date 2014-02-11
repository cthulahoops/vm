import Parse
import Compile
import System.Environment

main = do
    [fname] <- getArgs
    code   <- readFile fname
    stdlib <- readFile "lib/stdlib.ss"
    putStrLn $ (compile.parse) stdlib
    putStrLn ""
    putStrLn $ (compile.parse) code

parse x = let Right p = parseExprs x in p
