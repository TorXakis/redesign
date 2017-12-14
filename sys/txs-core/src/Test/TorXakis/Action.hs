module Test.TorXakis.Action
    ( Action (..)
    , ActionType (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String

-- | Actions
--
-- TODO: maybe we need an `Action a` type here, to abstract over the type of
-- actions.
data Action = Action { actionName :: Text }
            | Quiescence
    deriving (Eq)

instance Show Action where
    show (Action name) = show name
    show Quiescence = "Quiescence"

instance IsString Action where
    fromString = Action . T.pack -- Yes, yes, for now we could have Action "Quiescence". 

-- | Type of an action.
data ActionType = Input | Output deriving (Show, Eq)
