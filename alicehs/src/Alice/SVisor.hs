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
startSV
) where

import Alice.Servers.CommSrv as Comm
import Alice.Servers.AsSrv as As
import Alice.Servers.ContextSrv as Context
import Alice.Servers.ConsSrv as Cons
import Alice.Servers.RootSrv as Root

import Alice.Messages.CommMsg
import Alice.Messages.AsMsg
import Alice.Messages.ContextMsg
import Alice.Messages.ConsMsg
import Alice.Messages.RootMsg

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Control.Exception

startSV :: IO ()
startSV = do
   commQ <- atomically $ newTQueue
   asQ <- atomically $ newTQueue
   contextQ <- atomically $ newTQueue
   consQ <- atomically $ newTQueue
   rootQ <- atomically $ newTQueue

   errQ <- atomically $ newTQueue

   Cons.start consQ rootQ commQ
   As.start asQ rootQ contextQ
   Context.start contextQ rootQ consQ asQ
   Comm.start commQ rootQ asQ
   Root.start rootQ commQ asQ consQ

   waitError errQ

waitError :: TQueue String -> IO ()
waitError errQ = do
    errRes <- atomically $ tryReadTQueue errQ
    case errRes of
        Nothing -> do
            threadDelay $ 200*1000
            waitError errQ
        Just err -> do
            print err
            error err

