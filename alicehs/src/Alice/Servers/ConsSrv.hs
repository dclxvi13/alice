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
import qualified Alice.NameResolver as NR

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Alice.Common.Utils

import Data.Dynamic

data ConsState = ConsState {
  consState_self :: Queue,
  consState_nr :: NR.NR
}

start :: Queue -> (TQueue ErrMsg, NR.NR) -> IO ()
start selfQ (svQ, nrQ) = startActor svQ $ do
    state <- initCons selfQ nrQ
    loopCons state

initCons :: Queue -> NR.NR -> IO ConsState
initCons self nrQ = do
    print "Here start Cons"
    return $ ConsState self nrQ

loopCons :: ConsState -> IO ()
loopCons state = do
    msg <- receive $ consState_self state
    case msg of
        Just ConsMsg -> loopCons state
