module System.Log (module System.Log, module X) where

import System.Log.Level  as X
import System.Log.Logger as X
import System.Log.Data   as X (nested)



-- TODO
-- [ ] Tests
-- [ ] Docs + Examples
-- [ ] Threading loggers
-- [ ] FilterLogger



import Prologue
import System.Log.Logger.Format


-------------------
-- === Tests === --
-------------------

tst :: Logging m => m ()
tst = runIdentityT $ do
    withDebug "foo" $ do
        debug "bar"
    debug "baz"
    return ()


lmain :: IO ()
lmain = do
    dropLogs tst
    -- runTaggedLogging $ runEchoLogger $ runFormatLogger bulletNestingFormatter $ tst
    runTaggedLogging $ runEchoLogger $ runFormatLogger bulletNestingFormatter $ tst
    runTaggedLogging $ runEchoLogger $ plain $ runFormatLogger bulletNestingFormatter $ tst
    runPriorityLogging $ runPriorityLogger @StdLevels $ runEchoLogger $ runFormatLogger examplePriorityFormatter $ tst
    -- let logs = runIdentity $ runPriorityLogging $ execWriterLogger @Msg $ tst
    -- print "---"
    -- print logs
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst
    -- dropLogs tst
    putStrLn ""
    print "hello"
