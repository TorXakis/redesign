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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.TorXakis.Core where

-- TODO: distribute the functionality into different modules, if appropriate.
-- TODO: hide the irrelevant parts.

-- Standard library imports
import Control.Concurrent.Async
import Control.Monad.Extra
import Control.Concurrent.STM
import Control.Applicative ((<|>))
import Control.Concurrent
import System.Random
    
-- Related third party imports
import Pipes hiding (next)
    
-- Local application/library specific imports
import Test.TorXakis.Specification
    
-- | Connection to the external world.
class WorldConnection c where
    -- | Start the external world. It might include the starting the SUT.
    initConnection :: c -> IO ()

    -- | Receive an action from the external world. This action is blocking.
    fromWorld :: c -> IO Action

    -- | Send an action to the external world.
    toWorld :: c -> Action -> IO ()

    -- | Stop the external world.
    stop :: c -> IO ()

-- | Reports the actions that occur during testing.
class Reporter r where
    -- | Initialize the reporter
    initReporter :: r -> IO ()

    -- | Report an action.
    report :: r -> Observation -> IO ()

    -- | Stream of test output.
    output :: r -> Producer Observation IO ()

-- | Output that can be observed from in the testing process.
data Observation
    = Result Verdict
    | ObservedInput Action
    | ObservedOutput Action -- ^ Quiescence is also considered an output.
    deriving (Show)

hasVerdict :: Observation -> Bool
hasVerdict (Result Pass) = True
hasVerdict (Result Fail) = True
hasVerdict _ = False

-- | Transform an action to and observation.
actToObservation :: ActionType -> Action -> Observation
actToObservation Input = ObservedInput
actToObservation Output = ObservedOutput

-- | Keeps track of the current state of the specification.
class Bookkeeper b where -- TODO: maybe Bookkeeper needs to be renamed to Spec.
    -- | Initialize the bookkeeper.
    initBookeeper :: b -> TxsSpec -> IO () -- TODO: we might even abstract away
                                           -- from the TxsSpec (and rename
                                           -- "Bookkeeper" to "Specification"!)
    
    -- | Performs the given action, updating the specification accordingly.
    -- 
    -- The returned value is True iff the current action is allowed in the
    -- specification.
    --
    step :: b -> Action -> IO Verdict

    -- | Get a next input action if possible. Return Nothing if no input action
    -- is enabled.
    nextInputAction :: b -> IO (Maybe Action) -- TODO: Do we need IO at all?

    -- | Is the specification in a @pass@ or @fail@ state?
    verdict :: b -> IO Verdict -- TODO: again: is IO needed?

    actionType :: b -> Action -> ActionType

data Verdict
    = Pass
    | Fail
    | NoConclusion -- ^ No conclusion was reached, the test must proceed to
                   -- have a verdict.
    deriving (Show, Eq)

initTest :: (WorldConnection c, Bookkeeper b, Reporter r)
         => c -> b -> r -> TestParams -> TxsSpec -> IO (TesterHandle c b r)
initTest c b r params spec = do
    initConnection c
    initBookeeper b spec
    initReporter r
    -- Initially the test process is not active:
    activeTV <- newTVarIO False
    cmdsQ <- newTQueueIO
    let env = TestEnv activeTV cmdsQ c b r params
    tProc <- async $ cmdsHandler env cmdsQ
    let handle = TesterHandle tProc env
    return handle
  where
    -- TODO: consider handling asynchronous exceptions
    cmdsHandler env cmdsQ = forever $ do
        cmd <- atomically $ readTQueue cmdsQ
        case cmd of
            CmdTest sn -> testLoop sn env -- Note that the handler won't start
                                          -- a new loop till the current
                                          -- command is handled. (It doesn't
                                          -- make sense to test concurrently).


-- | Test environment.
--
-- NOTE: by using different kind of environments we remove the need for making
-- a case analysis of in which state the TorXakis core is.
data TestEnv c b r where
     -- TODO: consider using existential quantifications over GADT's.
    TestEnv :: (WorldConnection c, Bookkeeper b, Reporter r)
            => { active :: TVar Bool    -- ^ Is the tester active.
               , cmds :: TQueue Cmd     -- ^ Commands sent to the tester process.
               , connection :: c
               , bookkeeper :: b 
               , reporter :: r
               , params :: TestParams
               } -> TestEnv c b r

newtype Cmd = CmdTest StepsNumber deriving (Show)

data TesterHandle c b r = TesterHandle
    { testerProc :: Async () -- ^ Tester process
    , testEnv :: TestEnv c b r
    }

-- | Stepper environment.
--
-- A stepper does need a connection with the outside world.
newtype StepEnv = StepEnv TxsSpec

-- No SUT connection required here.
-- initStep :: TxsSpec -> IO StepEnv
-- initStep = 

-- | Number of steps to make.
data StepsNumber = All | Do Int deriving (Show)

decrement :: StepsNumber -> StepsNumber
decrement All = All
decrement (Do n) = Do (n - 1)

-- | Start testing: test case generation + conformance checking.
test :: StepsNumber -> TestEnv c b r -> IO ()
test sn TestEnv {active, cmds} = atomically $ do
    writeTQueue cmds (CmdTest sn)
    writeTVar active True

-- | Was a verdict already reached?
testDone :: TestEnv c b r -> IO Bool
testDone TestEnv {bookkeeper} = do
    v <- verdict bookkeeper 
    return $ v == Pass || v == Fail

-- | Is the test still active? (active == not suspended).
testActive :: TestEnv c b r -> IO Bool
testActive TestEnv {active} = readTVarIO active
    
-- | Test loop will be called by the main tester loop.
testLoop :: WorldConnection c
         => StepsNumber -> TestEnv c b r -> IO ()
testLoop n env@TestEnv{active} = do
    ifM (notM (testDone env) &&^ testActive env) 
        (do testStep env
            testLoop (decrement n) env
        )
        (atomically $ writeTVar active False)

newtype Milliseconds = Milliseconds { ms :: Int }
    deriving (Show, Eq, Num, Random)

-- | Convert the given number of milliseconds to microseconds.
inMicroseconds :: Milliseconds -> Int
inMicroseconds = (* 1000) . ms

data TestParams = TestParams
    { sutTimeout :: Milliseconds -- ^ Time to wait before deciding that
                                 -- the SUT is quiescent.
    }

data Spec = Spec

-- | Perform one test step.
testStep :: WorldConnection c => TestEnv c b r -> IO ()
testStep TestEnv{connection, bookkeeper, reporter, params} = do
    act <- runConcurrently $
        generateInput connection bookkeeper params
        <|>
        waitForOutput connection params
        <|>
        generateQuiescence (sutTimeout params)
    let t = actionType bookkeeper act
    report reporter (actToObservation t act)
    res <- step bookkeeper act
    report reporter (Result res) -- The no verdict is also reported, it is up
                                 -- to the reporter to decide whether to ignore
                                 -- this.
    
-- | Generate an input action based on the given specification.
--
-- If no input action is possible then the @Quiescence@ action is returned.
-- After waiting for the SUT timeout in the current state (given by the @Spec@
-- parameter).
--
generateInput :: (WorldConnection c, Bookkeeper b)
              => c -> b -> TestParams -> Concurrently Action
generateInput c b params = Concurrently $ do
    mAct <- nextInputAction b
    case mAct of
        Nothing -> waitForever
        Just act -> do
            -- Wait a random time before yielding an the action.
            -- The time to wait is in the range [0, sutTimeout params).
            wMs <- randomRIO (0, sutTimeout params)
            threadDelay (inMicroseconds wMs)
            toWorld c act
            return act
            
waitForever :: IO a
waitForever = do
    _ <- forever (threadDelay maxBound)
    undefined
            
-- | Waits for output indefinitely, if no output comes this function won't
-- return.
waitForOutput :: (WorldConnection c)
              => c -> TestParams -> Concurrently Action
waitForOutput conn _ = Concurrently $ fromWorld conn

-- | Generate a @Quiescence@ action after the given timeout.
generateQuiescence :: Milliseconds -> Concurrently Action
generateQuiescence ms = Concurrently $ do
    threadDelay (inMicroseconds ms)
    return Quiescence

suspendTest :: TestEnv c b r -> IO ()
suspendTest TestEnv {active} = atomically $ writeTVar active False


-- | Stop the test. Pending test steps are interrupted.
-- stopTest :: TesterHandle c b r -> IO ()
-- stopTest TesterHandle {} =   -- Terminate c, b, and r. Perform the cleanup here.
    
-- | Resume the test.
-- resumeTest = undefined
