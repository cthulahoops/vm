import Control.Applicative
import Control.Monad.State
import System.Console.Readline
import Vm
import Assemble
import Compile

main = do
        core   <- readFile "lib/core.s"
        stdlib <- compile.parse <$> readFile "lib/stdlib.ss"
        let lib = core ++ stdlib
        machine <- execStateT runMachine (newMachine (assemble lib))
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
