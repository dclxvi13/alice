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

import Alice.Common.DB

import Alice.Config (tickPeriod)
import qualified Alice.Workers.Ticker as T

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

data RootState = RootState {
    rootState_selfQ :: TQueue RootMsg,
    rootState_commQ :: TQueue CommMsg,
    rootState_asQ :: TQueue AsMsg,
    rootState_consQ :: TQueue ConsMsg,
    rootState_connection :: AliceDB,
    rootState_ticker :: T.Ticker
}

start :: TQueue RootMsg -> TQueue CommMsg -> TQueue AsMsg -> TQueue ConsMsg -> IO ThreadId
start selfQ commQ asQ consQ = forkIO $ do
    state <- initRoot selfQ commQ asQ consQ
    loopRoot state

initRoot :: TQueue RootMsg -> TQueue CommMsg -> TQueue AsMsg -> TQueue ConsMsg -> IO RootState
initRoot selfQ commQ asQ consQ = do
    print "Here start Root"

    ticker <- T.start tickPeriod commQ

    conn <- connectDB

    return RootState {
        rootState_ticker = ticker,
        rootState_asQ = asQ,
        rootState_commQ = commQ,
        rootState_consQ = consQ,
        rootState_selfQ = selfQ,
        rootState_connection = conn}

loopRoot :: RootState -> IO ()
loopRoot state = do
    msg <- atomically $ readTQueue $ rootState_selfQ state
    case msg of
        WordToForm word queue -> do
            form <- getFormByWord (rootState_connection state) word
            atomically $ writeTQueue queue form
            loopRoot state
        GetAssoc queue (first, second) -> do
            assocs <- getAssoc (rootState_connection state) first second
            atomically $ writeTQueue queue assocs
            loopRoot state
        GetEmo queue (first, second) currEmo ->
            undefined
