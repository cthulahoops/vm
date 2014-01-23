import System.Environment
import Assemble

main = do
    [fname] <- getArgs
    code <- readFile fname
    result <- run code
    print result

