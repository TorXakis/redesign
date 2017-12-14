module Test.TorXakis.Specification where
-- TODO: define the export list.

-- Standard library imports
import Data.Map (Map)
import Data.Text (Text)

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
--
-- TODO: maybe we need an `Action a` type here, to abstract over the type of
-- actions.
data Action = Action { actionName :: Text }
            | Quiescence
    deriving (Show, Eq)
    

-- | Type of an action.
data ActionType = Input | Output
