-----------------------------------------------------------------------------
--
-- Module      :  Alice.SVisor
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

module Alice.SVisor (
start
) where

import qualified Alice.Servers.CommSrv as Comm
import qualified Alice.Servers.AsSrv as As
import qualified Alice.Servers.ConsSrv as Cons
import qualified Alice.Servers.RootSrv as Root

import Alice.Messages.ErrMsg

import Alice.Common.Utils
import qualified Alice.NameResolver as NameR

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Dynamic

data SVisorState = SVisorState {
  sVisorState_children :: [SVChild],
  sVisorState_self :: TQueue ErrMsg,
  sVisorState_strategy :: SVStrategy,
  sVisorState_nameR :: NameR.NR
}

data SVStrategy = RestartOne | RestartAll

data SVChild = SVChild ThreadId Queue

type ChildFunc = Queue -> (TQueue ErrMsg, NameR.NR) -> IO ()

start :: IO ()
start = do
  errQ <- atomically newTQueue
  nrQ <- atomically newTQueue
  nr <- NameR.start nrQ

  let list = [("cons", Cons.start), ("as", As.start), ("comm", Comm.start), ("root", Root.start)]
      child = startChild (errQ,nrQ)
  children <- mapM child list

  let state = SVisorState {
    sVisorState_children = children,
    sVisorState_self = errQ,
    sVisorState_strategy = RestartOne,
    sVisorState_nameR = nrQ
  }

  loop state

startChild :: (TQueue ErrMsg, NameR.NR)
              -> (String, ChildFunc)
              -> IO SVChild
startChild (sv,nr) (name,func) = do
  queue <- atomically newTQueue
  tid <- forkIO $ func queue (sv,nr)
  NameR.setName nr name queue
  return $ SVChild tid queue

loop :: SVisorState -> IO ()
loop state = do
  let
    self = sVisorState_self state
    strategy = sVisorState_strategy state
    children = sVisorState_children state
  errRes <- atomically $ tryReadTQueue self
  case errRes of
      Nothing -> do
          yield
          loop state
      Just err -> do
          -- restart child
          loop state
