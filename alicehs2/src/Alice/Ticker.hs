module Alice.Ticker (
  TickReceiver,
  startTicker,
  getTick) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue

data TickerControlMsg = Stop
data Tick = Tick
  | Stopped

type Ticker = TQueue TickerControlMsg
type TickReceiver = TQueue Tick

tickReceiver :: IO TickReceiver
tickReceiver = atomically newTQueue

getTick :: TickReceiver -> IO Tick
getTick ticker = atomically $ readTQueue ticker

startTicker :: IO TickReceiver
startTicker = do
  (_, ticker, rcv) <- start 1000
  return rcv

start :: Int -> IO (ThreadId, Ticker, TickReceiver)
start period = do
  rcv <- tickReceiver
  ticker <- atomically newTQueue
  tid <- forkIO $ loop period rcv ticker
  return (tid, ticker, rcv)

loop :: Int -> TickReceiver -> Ticker -> IO ()
loop period rcv q = do
  threadDelay (period*1000)
  msg <- atomically $ tryReadTQueue q
  case msg of
    Nothing -> do
      atomically $ writeTQueue rcv Tick
      loop period rcv q
    Just Stop ->
      atomically $ writeTQueue rcv Stopped
