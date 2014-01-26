import Compile
import System.Environment

main = do
    [fname] <- getArgs
    code   <- readFile fname
    stdlib <- readFile "lib/stdlib.ss"
    core   <- readFile "lib/core.s"
    putStrLn core
    putStrLn $ (compile.parse) stdlib
    putStrLn $ (compile.parse) code
