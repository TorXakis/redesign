module Test.TorXakis.Specification where

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
data Action = Action { actionName :: Text }
            | Quiescence
    deriving (Show)
    

