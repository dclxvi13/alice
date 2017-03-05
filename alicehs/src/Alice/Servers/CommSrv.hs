-----------------------------------------------------------------------------
--
-- Module      :  Alice.Servers.CommSrv
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Alice.Servers.CommSrv (start) where

import Alice.Messages.CommMsg
import Alice.Messages.RootMsg
import Alice.Messages.AsMsg

import qualified Alice.Sensors.TextSensor as TS

import qualified Alice.Workers.CommWorker as CW

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

data CommState = CommState {
    commState_textSensor :: TQueue TS.TextSensorMsg
}

start :: TQueue CommMsg -> TQueue RootMsg -> TQueue AsMsg -> IO ThreadId
start selfQ rootQ asQ = forkIO $ do
        state <- initComm selfQ
        loopComm state selfQ rootQ asQ

initComm :: TQueue CommMsg -> IO CommState
initComm selfQ = do
    print "Here start Comm"

    textS <- TS.start selfQ

    return CommState {commState_textSensor = textS}

loopComm :: CommState -> TQueue CommMsg -> TQueue RootMsg -> TQueue AsMsg -> IO ()
loopComm state selfQ rootQ asQ = do
    msg <- atomically $ readTQueue selfQ
    case msg of
        Tick -> do
            --print "It is a tick, motherfucker!"
            atomically $ writeTQueue (commState_textSensor state) TS.GetTextData
            loopComm state selfQ rootQ asQ
        TextSensorClosed -> do
            -- do something
            loopComm state selfQ rootQ asQ
        TextData bytes -> do
            --print $ show bytes
            worker <- CW.start CW.Read rootQ asQ bytes

            loopComm state selfQ rootQ asQ
