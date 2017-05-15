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
import qualified Alice.NameResolver as NR

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
    rootState_nr :: NR.NR,
    rootState_ticker :: T.Ticker
}

start :: Queue -> (TQueue ErrMsg, TQueue NR.NRMsg) -> IO ()
start selfQ (svQ, nrQ) = startActor svQ $ do
    state <- initRoot selfQ nrQ
    loopRoot state

initRoot :: Queue -> NR.NR -> IO RootState
initRoot selfQ nrQ = do
    print "Here start Root"

    ticker <- T.start tickPeriod $ sendN nrQ "comm" Tick

    conn <- connectDB

    return RootState {
        rootState_ticker = ticker,
        rootState_selfQ = selfQ,
        rootState_nr = nrQ,
        rootState_connection = conn}

loopRoot :: RootState -> IO ()
loopRoot state = do
    msg <- receive $ rootState_selfQ state
    case msg of
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
