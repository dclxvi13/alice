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
import qualified Alice.NameResolver as NR

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
  commState_textSensor :: TQueue TS.TextSensorMsg,
  commState_nr :: TQueue NR.NRMsg
}

start :: TQueue Dynamic -> (TQueue ErrMsg, TQueue NR.NRMsg) -> IO ()
start selfQ (svQ, nrQ) = startActor svQ $ do
  state <- initComm selfQ nrQ
  loopComm state

initComm :: TQueue Dynamic -> TQueue NR.NRMsg -> IO CommState
initComm selfQ nrQ = do
    print "Here start Comm"

    textS <- TS.start selfQ

    return CommState {
      commState_self = selfQ,
      commState_textSensor = textS,
      commState_nr = nrQ
    }

loopComm :: CommState
    -> IO ()
loopComm state = do
    msg <- receive $ commState_self state
    case msg of
      Nothing -> print "ERROR: unable cast msg (Comm)"
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
          --print $ "CommSrv: " ++ show bytes
          worker <- CW.start CW.Read (commState_nr state) bytes

          loopComm state
