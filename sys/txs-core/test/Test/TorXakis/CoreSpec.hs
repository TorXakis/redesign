{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.TorXakis.CoreSpec where

import Data.Functor.Identity

import Test.Hspec

import Test.TorXakis.Core

data DummyConnection

instance WorldConnection DummyConnection Identity where
    start = const (return ())
    fromWorld _ = return (Action "dummy")
    toWorld _ _ = return ()
    stop = const (return ())
    
spec :: Spec
spec = do
    describe "Echo connection" $ do
        it "is not implemented " $ pendingWith "We need to implement this."
