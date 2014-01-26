import Vm
import Assemble
import Compile
import Control.Monad.State
import System.Console.Readline

main = do
        core <- readFile "lib/core.s"
        machine <- execStateT runMachine (newMachine (assemble core))
        loop machine
    where loop machine = do
                Just code <- readline ">>> "
                let program = (assemble.compile.parse) code
                (result, machine'') <- runStateT (runMachine >> popS) (machine {machineCP = program})
                print result
                loop machine''
