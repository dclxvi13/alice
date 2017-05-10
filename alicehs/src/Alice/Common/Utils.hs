module Alice.Common.Utils where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Dynamic

import Control.Exception

import Alice.Messages.ErrMsg

type Queue = TQueue Dynamic

startActor :: TQueue ErrMsg -> IO () -> IO ()
startActor svQ func = func `catch` (\err -> do
          self <- myThreadId
          atomically $ writeTQueue svQ $ ErrMsg self $ show (err :: SomeException))
