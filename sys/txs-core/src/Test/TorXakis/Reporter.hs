module Test.TorXakis.Reporter
    ( Reporter (..)
    , Observation (..)
    , hasVerdict
    , actToObservation
    )where

-- Related third party imports
import Pipes

-- Local application/library specific imports
import Test.TorXakis.Action
import Test.TorXakis.Verdict
    
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
