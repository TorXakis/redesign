module Test.TorXakis.Action
    ( Action (..)
    , ActionType (..)
    ) where

import Data.Text (Text)

-- | Actions
--
-- TODO: maybe we need an `Action a` type here, to abstract over the type of
-- actions.
data Action = Action { actionName :: Text }
            | Quiescence
    deriving (Show, Eq)

-- | Type of an action.
data ActionType = Input | Output deriving (Show, Eq)
