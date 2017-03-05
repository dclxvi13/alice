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
import Alice.Messages.ContextMsg

import Alice.Data.SForm
import Alice.Data.Emo

import qualified Alice.Workers.AsWorker as AW

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

data AsState = AsState {
    asState_selfQ :: TQueue AsMsg,
    asState_rootQ :: TQueue RootMsg,
    asState_contextQ :: TQueue ContextMsg,
    asState_current :: Emo
    }

start :: TQueue AsMsg -> TQueue RootMsg -> TQueue ContextMsg -> IO ThreadId
start selfQ rootQ contextQ = do
    forkIO $ do
        state <- initSrv selfQ rootQ contextQ
        loop state

initSrv :: TQueue AsMsg -> TQueue RootMsg -> TQueue ContextMsg -> IO AsState
initSrv selfQ rootQ contextQ = do
    print "Here start As"
    return $ AsState {
        asState_selfQ = selfQ,
        asState_rootQ = rootQ,
        asState_contextQ = contextQ,
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
    case msg of
        FromComm sforms -> do
            --print $ show sforms

            AW.start (asState_rootQ state) (asState_contextQ state) (asState_current state) sforms

            loop state
        UpdateCurrent emo -> do

            loop state{asState_current = emo}
