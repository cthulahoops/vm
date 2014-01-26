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
                readline ">>> " >>= \x -> case x of
                    Just line -> do
                        addHistory line
                        let program = (assemble.compile.parse) line
                        (result, machine'') <- runStateT (runMachine >> popS) (machine {machineCP = program})
                        print result
                        loop machine''
                    Nothing ->
                        return ()
