import Control.Applicative
import Control.Monad.State
import System.Console.Readline
import Vm
import VmTypes
import VmCode
import Compile
import Parse

main = do
        stdlib <- compile.parse <$> readFile "lib/stdlib.ss"
        let Right program = parseProgram stdlib
        machine <- execStateT runMachine (newMachine program)
        loop machine
    where loop machine = do
                readline ">>> " >>= \x -> case x of
                    Just line -> do
                        addHistory line
                        let Right program = (parseProgram.compile.parse) line
                        (result, machine'') <- runStateT (runMachine >> popS) (machine {machineCP = program})
                        print result
                        loop machine''
                    Nothing ->
                        return ()

parse x = let Right p = parseExprs x in p
