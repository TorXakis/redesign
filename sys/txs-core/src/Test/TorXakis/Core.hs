-----------------------------------------------------------------------------
-- |
-- Module      :  Test.TorXakis.Core
-- Copyright   :  (c) TNO-ESI and Radboud University
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.TorXakis.Core where

-- Standard library imports
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Monad.Extra

-- Related third party imports
import Pipes hiding (next)
    
-- Local application/library specific imports
import Test.TorXakis.Specification
    
-- | Connection to the external world.
class WorldConnection c where
    -- | Start the external world. It might include the starting the SUT.
    initConnection :: c -> IOC ()

    -- | Receive an action from the external world. This action is blocking.
    fromWorld :: c -> IOC Action

    -- | Send an action to the external world.
    toWorld :: c -> Action -> IOC ()

    -- | Stop the external world.
    stop :: c -> IOC ()

-- | Reports the actions that occur during testing.
class Reporter r where
    -- | Initialize the reporter
    initReporter :: IOC r

    -- | Report an action.
    report :: r -> Observation -> IOC ()

    -- ^ Stream of test output.
    output :: r -> ListT IOC Observation

-- | Output that can be observed from in the testing process.
data Observation
    = Result Verdict
    | ObservedInput Action
    | ObservedOutput Action
    | ObservedQuiescence
    deriving (Show)

-- | Keeps track of the current state of the specification.
class Bookkeeper b where
    -- | Initialize the bookkeeper.
    initBookeeper :: TxsSpec -> IOC b
    
    -- | Returns the next action according to the specification.
    nextAction :: b -> IOC Action

    -- | Performs the given action, updating the specification accordingly.
    -- 
    -- The returned value is True iff the current action is allowed in the
    -- specification.
    --
    step :: b -> Action -> IOC Verdict

data Verdict
    = Pass
    | Fail
    | NoConclusion -- ^ No conclusion was reached, the test must proceed to
                   -- have a verdict.
    deriving (Show)

-- | Core monad.
newtype IOC a = IOC { runIOC :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

initTest :: (WorldConnection c, Bookkeeper b, Reporter r)
         => c -> b -> r -> TxsSpec -> IOC (TestEnv c b r )
-- TODO: you might need to initialize the data structures here.
initTest c b r spec = IOC (return $ TestEnv c b r TestParams TesterHandle)

-- | Test environment.
--
-- NOTE: by using different kind of environments we remove the need for making
-- a case analysis of in which state the TorXakis core is.
data TestEnv c b r where
    TestEnv :: (WorldConnection c, Bookkeeper b, Reporter r)
        => c -> b -> r -> TestParams -> TesterHandle -> TestEnv c b r 

data TesterHandle = TesterHandle

doTest :: TesterHandle -> StepsNumber -> IOC ()
doTest = undefined

doStop :: TesterHandle -> IOC ()
doStop = undefined

testConn :: TestEnv c b r -> c
testConn (TestEnv c _ _ _ _) = c

testReporter :: TestEnv c b r -> r
testReporter (TestEnv _ _ r _ _) = r

-- | A handle to a running test. 

-- | Stepper environment.
--
-- A stepper does need a connection with the outside world.
newtype StepEnv = StepEnv TxsSpec

-- No SUT connection required here.
initStep :: TxsSpec -> IOC StepEnv
initStep = undefined

-- | Number of steps to make.
data StepsNumber = All | Do Int

-- | Start testing: test case generation + conformance checking.
test :: StepsNumber -> TestEnv c b r -> IOC ()
test n (TestEnv _ _ _ _ h) =
    doTest h n  -- This will queue the command using the handler.

-- | Was a verdict already reached?
testDone :: TestEnv c b r -> IOC Bool
testDone = undefined

-- | Is the test still active (not suspended?).
testActive :: TestEnv c b r -> IOC Bool
testActive = undefined
    
-- Test look will be called by the main tester loop
testLoop :: WorldConnection c
         => StepsNumber -> TestEnv c b r -> IOC ()    
testLoop All env = do
    done <- testDone env -- The testing process is finished.
    whenM (testActive env) (testStep env >> test All env)
testLoop _ _ = undefined

data TestParams = TestParams

data Spec = Spec

-- | Perform one test step.
testStep :: WorldConnection c => TestEnv c b r -> IOC ()
testStep (TestEnv c b r p h) = do
    ioAct <- liftIO $
        generateInput b p `race` waitForOutput c p
    res <- step b (either id id ioAct)
    case res of
        NoConclusion ->
            case ioAct of
                Left Quiescence -> report r ObservedQuiescence
                Left act -> report r (ObservedInput act)
                Right act -> report r (ObservedOutput act)                
        Pass -> report r (Result Pass)
        Fail -> report r (Result Fail)

    
-- | Generate an input action based on the given specification and test
-- parameters.
--
-- The test parameters determine how much time to wait before yielding an
-- input action (if any).
--
-- If no input action is possible then the @Quiescence@ action is returned.
-- After waiting for the SUT timeout in the current state (given by the @Spec@
-- parameter).
--
generateInput :: (Bookkeeper b)
              => b -> TestParams -> IO Action
generateInput b params = undefined

-- | Waits for output indefinitely, if no output comes this function won't
-- return.
waitForOutput :: (WorldConnection c)
              => c -> TestParams -> IO Action
waitForOutput = undefined

-- | Stop the test. Pending test steps are interrupted.
stopTest :: TestEnv c b r -> IOC ()
stopTest (TestEnv _ _ _ _ h) = doStop h -- Send a stop command to the test
    
-- | Resume the test.
resumeTest :: TestEnv c b r -> IOC ()
resumeTest = undefined
