import Control.Lens
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
                machine <- execVm (newMachine program)
                loop machine
            Left error ->
                print error
    where loop machine = do
                x <- readline ">>> "
                case x of 
                    Just line -> eval machine line >>= loop
                    Nothing   -> return ()
          eval machine line =
                case parseCompile $ "(begin (write " ++ line ++ ") (newline))" of
                    Right program ->
                        execVm (set machineCP program machine)
                    Left error -> do
                        print error
                        return machine

parseCompile code = compile <$> parseExprs code
