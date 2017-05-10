-----------------------------------------------------------------------------
--
-- Module      :  Alice.Servers.ConsSrv
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

module Alice.Servers.ConsSrv (start) where

import Alice.Messages.ConsMsg
import Alice.Messages.RootMsg
import Alice.Messages.CommMsg
import Alice.Messages.ErrMsg

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Alice.Common.Utils

import Data.Dynamic

data ConsState = ConsState {
  consState_self :: Queue
}

start :: Queue -> TQueue ErrMsg -> IO ()
start selfQ svQ = startActor svQ $ do
    state <- initCons selfQ
    loopCons state

initCons :: Queue -> IO ConsState
initCons self = do
    print "Here start Cons"
    return $ ConsState self

loopCons :: ConsState -> IO ()
loopCons state = do
    msg <- atomically $ readTQueue $ consState_self state
    case fromDynamic msg of
        Just ConsMsg -> loopCons state
