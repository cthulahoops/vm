import Control.Applicative
import Control.Monad.State
import System.Console.Readline
import SExprs
import Vm
import VmTypes
import VmCode
import Compile
import Parse

main = do
        stdlib <- readFile "lib/stdlib.ss"
        case parseCompile stdlib of
            Right program -> do
                machine <- execStateT runMachine (newMachine program)
                loop machine
            Left error ->
                print error
    where loop machine = do
                x <- readline ">>> "
                case x of 
                    Just line -> eval machine line >>= loop
                    Nothing   -> return ()
          eval machine line =
                case parseCompile line of
                    Right program -> do
                        (result, machine') <- runStateT (runMachine >> popS) (machine {machineCP = program})
                        print result
                        return machine'
                    Left error -> do
                        print error
                        return machine

parseCompile code = do
    p <- parseExprs code
    let mc = compile p
    parseProgram mc
