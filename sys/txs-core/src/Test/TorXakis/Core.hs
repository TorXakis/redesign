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
{-# LANGUAGE TypeFamilies #-}
module Test.TorXakis.Core where

-- Standard library imports
import Data.Map (Map)

-- Related third party imports

-- Local application/library specific imports

-- | Behavior expressions.
data BExpr

-- | Model definitions.
data ModelDef

-- | Process definitions.
data ProcDef = ProcDef BExpr

-- | Complete TorXakis specifications.
data TxsSpec = TxsSpec
    { procDefs :: Map (Id ProcDef) ProcDef
    , modelDefs :: Map (Id ModelDef) ModelDef
    }

-- | Identifiers for parts of the TorXakis specifications.
newtype Id v = Id { getId :: Int }

-- | Actions
data Action

-- | Connection to the external world.
class WorldConnection c where
    data M c
    -- | Start the external world. It might include the starting the SUT.
    start :: c -> M ()
    -- | Receive an action from the external world.
    fromWorld :: c -> M Action
    -- | Send an action to the external world.
    toWorld :: c -> Action -> M ()
    -- | Stop the external world.
    stop :: c -> M ()

-- | Core monad.
data IOC m

initTest :: WorldConnection c => TxsSpec -> c -> IOC (TestEnv c)
initTest = undefined

-- | Test environment.
--
-- NOTE: by using different kind of environments we remove the need for making
-- a case analysis of in which state the TorXakis core is.
data TestEnv c where
    TestEnv :: WorldConnection c => c -> TxsSpec -> TestEnv c

-- | Stepper environment.
--
-- A stepper does need a connection with the outside world.
data StepEnv = StepEnv TxsSpec

-- No SUT connection required here.
initStep :: TxsSpec -> IOC StepEnv
initStep = undefined

test :: TestEnv c -> IOC TestHandle
test = undefined

step :: StepEnv  -> IOC StepHandle
step = undefined

-- | A test handle contains:
--
-- - Current TorXakis specification being tested.
-- - A channel where the output is directed.
data TestHandle 

-- | Similar to a test handle.
data StepHandle
