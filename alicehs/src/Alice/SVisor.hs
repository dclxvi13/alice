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
--import qualified Alice.Servers.ContextSrv as Context
import qualified Alice.Servers.ConsSrv as Cons
import qualified Alice.Servers.RootSrv as Root

--import Alice.Messages.CommMsg
--import Alice.Messages.AsMsg
--import Alice.Messages.ContextMsg
--import Alice.Messages.ConsMsg
--import Alice.Messages.RootMsg
import Alice.Messages.ErrMsg

import Alice.Common.Utils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

--import Control.Exception
import Data.Dynamic

data SVisorState = SVisorState {
  sVisorState_children :: [SVChild],
  sVisorState_self :: TQueue ErrMsg,
  sVisorState_strategy :: SVStrategy
}

data SVStrategy = RestartOne | RestartAll

data SVChild = SVChild ThreadId Queue

type ChildFunc = Queue -> TQueue ErrMsg -> IO ()

start :: IO ()
start = do
  errQ <- atomically newTQueue

  --commQ <- atomically newTQueue
  --asQ <- atomically newTQueue
  --consQ <- atomically newTQueue
  --rootQ <- atomically newTQueue

  --Cons.start consQ rootQ commQ
  --As.start asQ rootQ contextQ
  --Comm.start commQ rootQ asQ
  --Root.start rootQ commQ asQ consQ

  let list = [Cons.start, As.start, Comm.start, Root.start]
  children <- mapM (startChild errQ) list

  let state = SVisorState children errQ RestartOne

  waitError state

startChild :: TQueue ErrMsg             --Supervisor's queue
              -> ChildFunc              --Function to start child
              -> IO SVChild
startChild errQ func = do
  queue <- atomically newTQueue
  tid <- forkIO $ func queue errQ
  return $ SVChild tid queue

waitError :: SVisorState -> IO ()
waitError state = do
  let
    self = sVisorState_self state
    strategy = sVisorState_strategy state
    children = sVisorState_children state
  errRes <- atomically $ tryReadTQueue self
  case errRes of
      Nothing -> do
          yield
          waitError state
      Just err -> do

          waitError state
