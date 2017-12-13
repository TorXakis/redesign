module Test.TorXakis.ConsoleRunner where

-- Standard library imports

-- Related third party imports
import Pipes

-- Local application/library specific imports
import Test.TorXakis.Core

-- | Run a test up to its completion.
runTest :: (WorldConnection c)
        => c -> TxsSpec -> IOC ()
runTest conn spec = do
    tEnv <- initTest conn spec
    th <- test All tEnv
    runEffect $ for (every (output th)) (liftIO . print)
    return ()
