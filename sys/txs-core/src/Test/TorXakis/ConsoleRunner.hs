module Test.TorXakis.ConsoleRunner where

-- Standard library imports

-- Related third party imports
import Pipes

-- Local application/library specific imports
import Test.TorXakis.Core
import Test.TorXakis.Specification

-- | Run a test up to its completion.
runTest :: (WorldConnection c, Bookkeeper b, Reporter r)
        => c -> b -> r -> TxsSpec -> IO ()
runTest c b r spec = do
    th <- initTest c b r spec
    let env = testEnv th
    test All env
    let testOutput = output (reporter env)
    runEffect $ for (every testOutput) (liftIO . print) -- TODO: you can
                                                        -- abstract this away
                                                        -- into some action of
                                                        -- the tester.
