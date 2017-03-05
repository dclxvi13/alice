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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

start :: TQueue ConsMsg -> TQueue RootMsg -> TQueue CommMsg -> IO ThreadId
start selfQ rootQ commQ = forkIO $ do
    initCons
    loopCons selfQ rootQ commQ

initCons :: IO ()
initCons = do
    print "Here start Cons"

loopCons :: TQueue ConsMsg -> TQueue RootMsg -> TQueue CommMsg -> IO ()
loopCons selfQ rootQ commQ = do
    msg <- atomically $ readTQueue selfQ
    case msg of
        ConsMsg -> loopCons selfQ rootQ commQ
