-----------------------------------------------------------------------------
--
-- Module      :  Alice.Workers.Ticker
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

module Alice.Workers.Ticker (
start,
Ticker,
TickerMsg (..)
) where

import Alice.Messages.CommMsg

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

data TickerMsg = TickerStop

type Ticker = TQueue TickerMsg

start :: Int -> IO () -> IO (TQueue TickerMsg)
start tickTime func = do
    q <- atomically newTQueue
    forkIO $ loopTicker q tickTime func
    return q

loopTicker :: TQueue TickerMsg -> Int -> IO () -> IO ()
loopTicker q tickTime func = do
        threadDelay tickTime
        msg <- atomically $ tryReadTQueue q
        case msg of
            Nothing -> do
                func
                loopTicker q tickTime func
            Just TickerStop -> return ()
