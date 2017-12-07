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
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.TorXakis.Core where

-- Standard library imports
import Data.Map (Map)
import Data.Text (Text)
    
-- Related third party imports

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
newtype Action = Action { actionName :: Text }

-- | Connection to the external world.
class Monad m => WorldConnection c m where
    -- | Start the external world. It might include the starting the SUT.
    start :: c -> m ()
    -- | Receive an action from the external world.
    fromWorld :: c -> m Action
    -- | Send an action to the external world.
    toWorld :: c -> Action -> m ()
    -- | Stop the external world.
    stop :: c -> m ()

-- | Core monad.
data IOC m = IOC m

initTest :: WorldConnection c m => c -> TxsSpec -> IOC (TestEnv c m)
initTest c spec = IOC (TestEnv c spec)

-- | Test environment.
--
-- NOTE: by using different kind of environments we remove the need for making
-- a case analysis of in which state the TorXakis core is.
data TestEnv c m where
    TestEnv :: WorldConnection c m => c -> TxsSpec -> TestEnv c m

-- | Stepper environment.
--
-- A stepper does need a connection with the outside world.
newtype StepEnv = StepEnv TxsSpec

-- No SUT connection required here.
initStep :: TxsSpec -> IOC StepEnv
initStep = undefined

test :: TestEnv c m -> IOC TestHandle
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
