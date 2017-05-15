module Alice.Common.Utils where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Dynamic
import Control.Exception
import Alice.Messages.ErrMsg
import Alice.NameResolver

import qualified Data.Map.Strict as Map

type Queue = TQueue Dynamic

startActor :: TQueue ErrMsg -> IO () -> IO ()
startActor svQ func = func `catch` (\err -> do
          self <- myThreadId
          atomically $ writeTQueue svQ $ ErrMsg self $ show (err :: SomeException))

send :: Typeable a => Queue -> a -> IO ()
send queue msg = atomically $ writeTQueue queue $ toDyn msg

sendN :: Typeable a => TQueue NRMsg -> String -> a -> IO ()
sendN nr name msg = atomically $ writeTQueue nr $ ProxyV name $ toDyn msg

receive :: Typeable a => Queue -> IO (Maybe a)
receive queue = do
  msg <- atomically $ readTQueue queue
  return $ fromDynamic msg
