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

--import Alice.Data.SForm
import Alice.Data.Emo

import qualified Alice.Workers.AsWorker as AW

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

--import Control.Exception

import Data.Dynamic

data AsState = AsState {
    asState_selfQ :: Queue,
    asState_current :: Emo
    }

start :: Queue -> TQueue ErrMsg  -> IO ()
start selfQ svQ = startActor svQ $ do
        state <- initSrv selfQ
        loop state

initSrv :: Queue -> IO AsState
initSrv selfQ = do
    print "Here start As"
    return AsState {
        asState_selfQ = selfQ,
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
    msg <- atomically $ readTQueue $ asState_selfQ state
    case fromDynamic msg of
        Just (FromComm sforms) ->

            loop state
        Just (UpdateCurrent emo) ->

            loop state{asState_current = emo}
