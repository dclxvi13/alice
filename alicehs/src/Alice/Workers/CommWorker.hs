-----------------------------------------------------------------------------
--
-- Module      :  Alice.Workers.CommWorker
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

module Alice.Workers.CommWorker (
start,
CommWorkerDirection(Read, Write)
) where

import Alice.Messages.RootMsg
import Alice.Messages.AsMsg

import Alice.Data.SForm
import Alice.Data.Form

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.BERT.Term
import Data.BERT.Parser

import qualified Data.ByteString.Char8 as B

data CommWorkerDirection = Read | Write

start :: CommWorkerDirection -> TQueue RootMsg -> TQueue AsMsg -> B.ByteString -> IO ThreadId
start Read rootQ asQ bytes = forkIO $ do
    selfQ <- atomically newTQueue
    case parseTerm $ B.unpack bytes of
        Right bert ->
          case readBERT bert :: Either String (String, [String]) of
            Right ("messages", []) -> return ()
            Right ("messages", msgs) -> do
              -- do something
              print $ show msgs
            Right val -> print $ "wrong term: " ++ show val
            Left readErr -> print $ "read error: " ++ readErr
        Left err -> print $ "error: " ++ show err
start Write rootQ asQ bytes = forkIO $ do
    undefined

askRoot :: TQueue Form -> TQueue RootMsg -> String -> IO Form
askRoot selfQ rootQ word = do
    atomically $ writeTQueue rootQ $ WordToForm word selfQ
    atomically $ readTQueue selfQ
