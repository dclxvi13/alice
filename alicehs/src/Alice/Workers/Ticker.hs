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

start :: Int -> TQueue CommMsg -> IO (TQueue TickerMsg)
start tickTime commQ = do
    q <- atomically $ newTQueue
    forkIO $ do
        loopTicker q tickTime commQ
    return q

loopTicker :: TQueue TickerMsg -> Int -> TQueue CommMsg -> IO ()
loopTicker q tickTime commQ = do
        threadDelay tickTime
        msg <- atomically $ tryReadTQueue q
        case msg of
            Nothing -> do
                atomically $ writeTQueue commQ Tick
                loopTicker q tickTime commQ
            Just TickerStop -> return ()

