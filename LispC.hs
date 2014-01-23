import Compile
import System.Environment

main = do
    [fname] <- getArgs
    code <- readFile fname
    core <- readFile "lib/core.s"
    putStrLn core
    putStrLn $ (compile.parse) code
