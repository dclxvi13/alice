-----------------------------------------------------------------------------
--
-- Module      :  Alice.Servers.RootSrv
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

module Alice.Servers.RootSrv (start) where

import Alice.Messages.RootMsg
import Alice.Messages.CommMsg
import Alice.Messages.AsMsg
import Alice.Messages.ConsMsg
import Alice.Messages.ErrMsg

import Alice.Common.DB

import Alice.Config (tickPeriod)
import qualified Alice.Workers.Ticker as T

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Dynamic
import Alice.Common.Utils

data RootState = RootState {
    rootState_selfQ :: Queue,
    rootState_connection :: AliceDB,
    rootState_ticker :: T.Ticker
}

start :: Queue -> TQueue ErrMsg -> IO ()
start selfQ svQ = startActor svQ $ do
    state <- initRoot selfQ
    loopRoot state

initRoot :: Queue -> IO RootState
initRoot selfQ = do
    print "Here start Root"

    --ticker <- T.start tickPeriod commQ

    conn <- connectDB

    return RootState {
        --rootState_ticker = ticker,
        rootState_selfQ = selfQ,
        rootState_connection = conn}

loopRoot :: RootState -> IO ()
loopRoot state = do
    msg <- atomically $ readTQueue $ rootState_selfQ state
    case fromDynamic msg of
        Just (WordToForm word queue) -> do
            form <- getFormByWord (rootState_connection state) word
            atomically $ writeTQueue queue form
            loopRoot state
        Just (GetAssoc queue (first, second)) -> do
            assocs <- getAssoc (rootState_connection state) first second
            atomically $ writeTQueue queue assocs
            loopRoot state
        Just (GetEmo queue (first, second) currEmo) ->
            undefined
