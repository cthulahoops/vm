import System.Environment
import VmCode
import Vm

main = do
    [fname] <- getArgs
    code <- readFile fname
    case (parseProgram code) of
        Right parsed -> runProgram parsed >>= print
        Left error   -> putStr "VmCode parse error: " >> print error
