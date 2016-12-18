module System.Log (module System.Log, module X) where

import System.Log.Level  as X
import System.Log.Logger as X
import System.Log.Data   as X (nested)



-- TODO
-- [ ] Tests
-- [ ] Docs + Examples
-- [ ] Threading loggers


import Prologue
import System.Log.Logger.Format
import System.Log.Data
import GHC.Stack



-------------------
-- === Tests === --
-------------------

tst :: Logging m => m ()
tst = runIdentityT $ do
    withDebug "foo" $ do
        warning "bar"
    debug "baz"
    return ()


lmain :: IO ()
lmain = do
    putStrLn "-- 1 --"
    dropLogs tst
    putStrLn "-- 2 --"
    runTaggedLogging $ runEchoLogger $ runFormatLogger bulletNestingFormatter $ tst
    putStrLn "-- 3 --"
    runTaggedLogging $ runEchoLogger $ plain $ runFormatLogger bulletNestingFormatter $ tst
    putStrLn "-- 4 --"
    runTaggedLogging $ runEchoLogger $ plain $ runFormatLogger bulletNestingFormatter $ runStaticTagFilterLogger @(StdPriorities Warning) tst
    putStrLn "-- 5 --"
    runTaggedLogging $ runEchoLogger $ plain $ runFormatLogger bulletNestingFormatter $ runDynamicTagFilterLogger (stdPriorities Warning) tst
    putStrLn "-- 6 --"
    runPriorityLogging @StdLevels $ runEchoLogger $ runFormatLogger examplePriorityFormatter $ tst
    putStrLn "-- 7 --"
    let logs = runIdentity $ runTaggedLogging $ execWriterLogger @Msg $ tst
    print logs
