module Test.TorXakis.Verdict
    ( Verdict (..)
    ) where

data Verdict
    = Pass
    | Fail
    | NoConclusion -- ^ No conclusion was reached, the test must proceed to
                   -- have a verdict.
    deriving (Show, Eq)
