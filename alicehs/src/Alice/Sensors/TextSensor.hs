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

import qualified Network.Simple.TCP as Net

import qualified Alice.Config as Config
import Alice.Messages.CommMsg
import Alice.Common.Utils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue()

import qualified Data.ByteString.Char8 as B
import Data.Dynamic

import System.Timeout (timeout)

data TextSensorMsg = GetTextData

start :: Queue -> IO (TQueue TextSensorMsg)
start commQ = do
    q <- atomically newTQueue

    _ <- forkIO $ initT q commQ

    return q


initT :: TQueue TextSensorMsg -> Queue -> IO ()
initT q commQ = Net.connect Config.adrrStr (show Config.port)
    $ \(socket, remoteAddr) -> loop (q, commQ, socket, remoteAddr)

loop :: (TQueue TextSensorMsg, Queue, Net.Socket, Net.SockAddr ) -> IO ()
loop (q, commQ, socket, remoteAddr) = do
    msg <- atomically $ readTQueue q
    case msg of
        GetTextData -> do
            let bytes = B.pack Config.storeStr
            print "Sending"
            Net.send socket bytes

            _t <- timeout 10000000 $ do
              res <- Net.recv socket Config.packetLength
              case res of
                Just a -> do
                  atomically $ writeTQueue commQ $ toDyn $ TextData a
                  return 0
                Nothing -> return 1

            case _t of
              Nothing -> do
                print "Timeout reached"
                sendClosed commQ
              Just 0 -> loop (q, commQ, socket, remoteAddr)
              Just _ -> do
                print "Error in TextSensor"
                sendClosed commQ

sendClosed :: Queue -> IO ()
sendClosed q = atomically $ writeTQueue q $ toDyn TextSensorClosed
