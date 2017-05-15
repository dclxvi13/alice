-----------------------------------------------------------------------------
--
-- Module      :  Alice.Workers.AsWorker
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

module Alice.Workers.AsWorker where

import Alice.Messages.RootMsg

import Alice.Data.SForm
import Alice.Data.Form
import Alice.Data.GetAssocsResult
import Alice.Data.Emo
import Alice.Data.ContextUnit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Control.Monad

start :: TQueue RootMsg -> Emo -> [SForm] -> IO ThreadId
start rootQ currEmo sforms = forkIO $ do
    undefined
