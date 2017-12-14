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
    , passing :: TVar Bool          -- ^ Is the test passing so far?
    }

mkSeqChecker :: [Action] -> IO SeqChecker
mkSeqChecker xs = atomically $ do
    expsTQ <- newTQueue
    traverse_ (writeTQueue expsTQ) xs
    passTV <- newTVar True -- No input action is seen at the beginning. The test is passing so far.
    return $ SeqChecker expsTQ passTV

-- An example...
mSeqCheck = mkSeqChecker [Action "Foo", Action "Bar", Action "Baz"]

data SimpleReporter = SimpleReporter
    {observed :: TChan Observation}

mkSimpleReporter :: IO SimpleReporter
mkSimpleReporter = do
    obsTC <- newTChanIO
    return $ SimpleReporter obsTC

instance Reporter SimpleReporter where
    initReporter = const $ return ()    

    report SimpleReporter{observed} = atomically . writeTChan observed

    output SimpleReporter{observed} = do
        observedDup <- lift $ atomically $ dupTChan observed
        loop observedDup
        where
          loop :: TChan Observation -> Producer Observation IO ()
          loop tchan = do
            obs <- lift $ atomically $ readTChan tchan
            yield obs
            unless (hasVerdict obs) (loop tchan)

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

-- | A specification as a sequence of expected actions.
data SeqSpec = SeqSpec
    { specTQ :: TQueue Action
    , verdTV :: TVar Verdict
    }

mkSeqSpec :: [Action] -> IO SeqSpec
mkSeqSpec xs = atomically $ do
    specTQ <- newTQueue
    traverse_ (writeTQueue specTQ) xs
    verdTV <- newTVar NoConclusion
    return $ SeqSpec specTQ verdTV

instance Bookkeeper SeqSpec where
    initBookeeper _ _ = return ()

    step SeqSpec{specTQ, verdTV} act = atomically $ do
        checkFail `orElse` checkEmpty `orElse` checkAct
        where
          checkFail = do
              verd <- readTVar verdTV
              if verd == Fail || verd == Pass -- A verdict was reached, no action is expected
                  then return Fail
                  else retry

          checkEmpty =
              ifM (isEmptyTQueue specTQ)
                  reportFail
                  retry    

          checkAct = do
              nextAct <- readTQueue specTQ
              if nextAct /= act
                  then reportFail
                  else ifM (isEmptyTQueue specTQ)
                           reportPass
                           (return NoConclusion)
          
          reportFail = writeTVar verdTV Fail >> return Fail

          reportPass = writeTVar verdTV Fail >> return Pass

    nextInputAction spec@SeqSpec{specTQ} = atomically $ do
        mNextAct <- tryPeekTQueue specTQ
        case mNextAct of
            Nothing -> return Nothing
            Just act -> if actionType spec act == Input
                        then return (Just act)
                        else return Nothing

    actionType _ (Action "OK") = Output
    actionType _ (Action "NOK") = Output
    actionType _ Quiescence = Output
    actionType _ (Action _) = Input

    verdict SeqSpec{verdTV} = atomically $ readTVar verdTV

    
-- | Run a test up to its completion.
runTest :: (WorldConnection c, Bookkeeper b, Reporter r)
        => c -> b -> r -> TxsSpec -> IO ()
runTest c b r spec = do
    th <- initTest c b r (TestParams 2000) spec
    let env = testEnv th
    test All env
    let testOutput = output (reporter env)
    runEffect $ for testOutput (liftIO . print) -- TODO: you can
                                                -- abstract this away
                                                -- into some action of
                                                -- the tester.
