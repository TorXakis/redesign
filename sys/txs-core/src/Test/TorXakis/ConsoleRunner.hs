{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.TorXakis.ConsoleRunner where

-- Standard library imports
import Control.Concurrent.STM
import Control.Monad.Extra
import Data.Foldable

-- Related third party imports
import Pipes

-- Local application/library specific imports
import Test.TorXakis.Core
import Test.TorXakis.Specification

-- | Simple world connection that has a list of expected input actions. For
-- each action that arrives in the expected order, an action "OK" is returned.
-- If an unexpected action arrives, then the "NOK" action is returned, and this
-- world connection will keep on repeating that output.
data SeqChecker = SeqChecker 
    { expectations :: TQueue Action -- ^ Sequence of expected input actions.
    , passing :: TVar (Bool)        -- ^ Is the test passing so far?
    }

mkSeqChecker :: [Action] -> IO SeqChecker
mkSeqChecker xs = atomically $ do
    expsTQ <- newTQueue
    traverse_ (writeTQueue expsTQ) xs
    passTV <- newTVar True -- No input action is seen at the beginning. The test is passing so far.
    return $ SeqChecker expsTQ passTV

instance WorldConnection SeqChecker where
    initConnection = const $ return ()

    fromWorld SeqChecker{passing} = atomically $
        ifM (readTVar passing)
            (return (Action "OK"))
            (return (Action "NOK"))

    toWorld SeqChecker{expectations, passing} act = atomically $
        whenM (readTVar passing) $ do
        -- Match the received action to the expectations.
            expAct <- readTQueue expectations
            writeTVar passing (act == expAct)

    stop = const $ return ()        
        
-- | Run a test up to its completion.
runTest :: (WorldConnection c, Bookkeeper b, Reporter r)
        => c -> b -> r -> TxsSpec -> IO ()
runTest c b r spec = do
    th <- initTest c b r (TestParams 2000)spec
    let env = testEnv th
    test All env
    let testOutput = output (reporter env)
    runEffect $ for (every testOutput) (liftIO . print) -- TODO: you can
                                                        -- abstract this away
                                                        -- into some action of
                                                        -- the tester.
