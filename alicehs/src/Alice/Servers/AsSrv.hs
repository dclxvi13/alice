-----------------------------------------------------------------------------
--
-- Module      :  Alice.Servers.AsSrv
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

module Alice.Servers.AsSrv (start) where

import Alice.Messages.AsMsg
import Alice.Messages.RootMsg
import Alice.Messages.ErrMsg
import Alice.Common.Utils
import qualified Alice.NameResolver as NR

import Alice.Data.Emo

import qualified Alice.Workers.AsWorker as AW

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Dynamic

data AsState = AsState {
    asState_selfQ :: Queue,
    asState_current :: Emo,
    asState_nr :: NR.NR
    }

start :: Queue -> (TQueue ErrMsg, TQueue NR.NRMsg)  -> IO ()
start selfQ (svQ, nrQ) = startActor svQ $ do
        state <- initSrv selfQ nrQ
        loop state

initSrv :: Queue -> NR.NR -> IO AsState
initSrv selfQ nrQ = do
    print "Here start As"
    return AsState {
        asState_selfQ = selfQ,
        asState_nr = nrQ,
        asState_current = Emo {
            emo_Ad = 0 ,
            emo_Co = 0 ,
            emo_Do = 0 ,
            emo_En = 0 ,
            emo_Ox = 0 ,
            emo_Ph = 0 }
    }

loop :: AsState -> IO ()
loop state = do
    msg <- receive $ asState_selfQ state
    case msg of
        Just (FromComm forms) -> do
            print $ "AsSrv: " ++ show forms
            loop state
        Just (UpdateCurrent emo) -> do

            loop state{asState_current = emo}
