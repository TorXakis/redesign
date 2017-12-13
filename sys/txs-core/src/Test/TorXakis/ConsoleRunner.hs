module Test.TorXakis.ConsoleRunner where

-- Standard library imports

-- Related third party imports
import Pipes

-- Local application/library specific imports
import Test.TorXakis.Core
import Test.TorXakis.Specification

-- | Run a test up to its completion.
runTest :: (WorldConnection c, Bookkeeper b, Reporter r)
        => c -> b -> r -> TxsSpec -> IOC ()
runTest c b r spec = do
    tEnv <- initTest c b r spec
    test All tEnv
    let testOutput = output (testReporter tEnv)
    runEffect $ for (every testOutput) (liftIO . print) -- TODO: you can
                                                        -- abstract this away
                                                        -- into some action of
                                                        -- the tester.
    return ()
