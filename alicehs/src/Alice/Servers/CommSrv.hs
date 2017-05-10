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
import Alice.Messages.ErrMsg
import Alice.Common.Utils

import qualified Alice.Sensors.TextSensor as TS

import qualified Alice.Workers.CommWorker as CW

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

--import Control.Exception

import Data.ByteString
import Data.Dynamic

data CommState = CommState {
  commState_self :: TQueue Dynamic,
  commState_textSensor :: TQueue TS.TextSensorMsg
}

--start :: TQueue CommMsg -> TQueue RootMsg -> TQueue AsMsg -> IO ThreadId
--start selfQ rootQ asQ = forkIO $ do
--        state <- initComm selfQ
--        loopComm state selfQ rootQ asQ

start :: TQueue Dynamic -> TQueue ErrMsg -> IO ()
start selfQ svQ = startActor svQ $ do
  state <- initComm selfQ
  loopComm state

initComm :: TQueue Dynamic -> IO CommState
initComm selfQ = do
    print "Here start Comm"

    textS <- TS.start selfQ

    return CommState {commState_self = selfQ, commState_textSensor = textS}

loopComm :: CommState
    -> IO ()
loopComm state = do
    msg <- atomically $ readTQueue $ commState_self state
    case fromDynamic msg of
      Nothing -> undefined
      Just Tick -> do
          --print "It is a tick, motherfucker!"
          atomically $ writeTQueue (commState_textSensor state) TS.GetTextData
          loopComm state
      Just TextSensorClosed -> do
          -- restart text sensor
          textS <- TS.start $ commState_self state
          let state' = state {commState_textSensor = textS}

          loopComm state'
      Just (TextData bytes) -> do
          --print $ show bytes
          --worker <- CW.start CW.Read rootQ asQ bytes

          loopComm state
