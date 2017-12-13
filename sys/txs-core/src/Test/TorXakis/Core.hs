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
import Data.Map (Map)
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Applicative ((<|>))
import Data.Maybe
    
-- Related third party imports
import Pipes hiding (next)
    
-- Local application/library specific imports

-- | Behavior expressions.
data BExpr

-- | Model definitions.
data ModelDef

-- | Process definitions.
newtype ProcDef = ProcDef BExpr

-- | Complete TorXakis specifications.
data TxsSpec = TxsSpec
    { procDefs :: !(Map (Id ProcDef) ProcDef)
    , modelDefs :: !(Map (Id ModelDef) ModelDef)
    }

-- | Identifiers for parts of the TorXakis specifications.
newtype Id v = Id { getId :: Int }

-- | Actions
newtype Action = Action { actionName :: Text } deriving (Show)

-- | Test action: actions observed during tests.
data TestAction = Quiescence    -- ^ Quiescence was observed.
                | Output Action -- ^ An output action was observed.
                | Input Action  -- ^ An input action was sent to the SUT.

-- | Connection to the external world.
class WorldConnection c where
    -- | Start the external world. It might include the starting the SUT.
    start :: c -> IOC ()
    -- | Receive an action from the external world.
    fromWorld :: c -> IOC Action
    -- | Send an action to the external world.
    toWorld :: c -> Action -> IOC ()
    -- | Stop the external world.
    stop :: c -> IOC ()

-- | Core monad.
newtype IOC a = IOC { runIOC :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

initTest :: WorldConnection c => c -> TxsSpec -> IOC (TestEnv c)
initTest c spec = IOC (return $ TestEnv c spec)

-- | Test environment.
--
-- NOTE: by using different kind of environments we remove the need for making
-- a case analysis of in which state the TorXakis core is.
data TestEnv c where
    TestEnv :: WorldConnection c => c -> TxsSpec -> TestEnv c

testConn :: TestEnv c -> c
testConn (TestEnv c _ ) = c

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
test :: StepsNumber -> TestEnv c -> IOC TestHandle
test All env = do
    let conn = testConn env
    undefined
test _ _ = undefined

data TestParams = TestParams

data Spec = Spec

-- | Perform one test step.
testStep :: WorldConnection c => TestParams -> Spec -> c -> IOC Spec
testStep params spec conn = do
    act <- liftIO $ runConcurrently $
        generateInput params conn spec <|> waitForOutput params spec conn
    next act spec
    
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
generateInput :: WorldConnection c => TestParams -> c -> Spec -> Concurrently TestAction
generateInput params c spec = undefined

-- | Waits for output indefinitely, if no output comes this function won't
-- return.
waitForOutput :: WorldConnection c => TestParams -> Spec -> c -> Concurrently TestAction
waitForOutput = waitForOutput

-- | Update the spec to account for the fact that the given action took place.
next :: TestAction -> Spec -> IOC Spec
next = undefined

-- | Stop the test. Pending test steps are interrupted.
stopTest :: TestHandle -> IOC ()
stopTest = undefined

-- | Resume the test.
resumeTest :: TestHandle -> IOC ()
resumeTest = undefined

step :: StepEnv  -> IOC StepHandle
step = undefined

-- | A test handle contains:
--
-- - Current TorXakis specification being tested.
-- - A channel where the output is directed.
--
data TestHandle = TestHandle
    { output :: ListT IOC TestOutput -- ^ Stream of test output.
    }

-- | Output that can be observed from in the testing process.
data TestOutput = Pass | Fail | FromWorld Action | ToWorld Action
    deriving Show

-- | Similar to a test handle.
data StepHandle
