module Test.TorXakis.Specification
    ( Specification (..)
    ) where

-- Standard library imports

-- Local application/library specific imports
import Test.TorXakis.Action
import Test.TorXakis.Verdict

-- | Keeps track of the current state of the specification.
class Specification b where
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
