import Parse
import Compile
import System.Environment

main = do
    args <- getArgs
    code <- case args of
        []      -> getContents
        [fname] -> readFile fname
    stdlib <- readFile "lib/stdlib.ss"
    putStrLn $ (compile.parse) stdlib
    putStrLn ""
    case parseExprs code of
        Right p ->
            putStrLn $ compile p
        Left error -> do
            putStrLn "Parse Error"
            putStrLn $ show error

parse x = let Right p = parseExprs x in p
