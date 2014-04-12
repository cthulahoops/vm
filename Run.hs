import System.Environment
import VmCode
import Vm

main = do
    args <- getArgs
    code <- case args of
        []      -> getContents
        [fname] -> readFile fname
    case (parseProgram code) of
        Right parsed -> runProgram parsed >> return ()
        Left error   -> putStr "VmCode parse error: " >> print error
