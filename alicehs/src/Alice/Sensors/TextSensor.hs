-----------------------------------------------------------------------------
--
-- Module      :  Alice.Sensors.TextSensor
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

module Alice.Sensors.TextSensor (
start,
TextSensorMsg(
    GetTextData
)
) where

import Network.Simple.TCP

import qualified Alice.Config as Config
import Alice.Messages.CommMsg

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue()

import qualified Data.ByteString.Char8 as B

data TextSensorMsg = GetTextData

start :: TQueue CommMsg -> IO (TQueue TextSensorMsg)
start commQ = do
    q <- atomically newTQueue

    _ <- forkIO $ initT q commQ

    return q


initT :: TQueue TextSensorMsg -> TQueue CommMsg -> IO ()
initT q commQ = connect Config.adrrStr Config.portStr $
      \(socket, remoteAddr) -> loop (q, commQ, socket, remoteAddr)

loop :: (TQueue TextSensorMsg, TQueue CommMsg, Socket, SockAddr) -> IO ()
loop (q, commQ, socket, remoteAddr) = do
    msg <- atomically $ readTQueue q
    case msg of
        GetTextData -> do
            let bytes = B.pack Config.storeStr
            send socket bytes
            text <- recv socket Config.packetLength
            case text of
                Nothing -> atomically $ writeTQueue commQ TextSensorClosed
                Just b -> do
                    atomically $ writeTQueue commQ $ TextData b
                    loop (q, commQ, socket, remoteAddr)
