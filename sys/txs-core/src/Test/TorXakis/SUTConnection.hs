module Test.TorXakis.SUTConnection
    ( SUTConnection (..)
    )where

import Test.TorXakis.Action

-- | Connection to the SUT.
class SUTConnection c where
    -- | Start the SUT.
    startSUT :: c -> IO ()

    -- | Receive an action from the external world. This action is blocking.
    fromSUT :: c -> IO Action

    -- | Send an action to the external world.
    toSUT :: c -> Action -> IO ()

    -- | Stop the SUT.
    stopSUT :: c -> IO ()

