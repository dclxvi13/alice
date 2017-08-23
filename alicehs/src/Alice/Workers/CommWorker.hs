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
import qualified Alice.NameResolver as NR
import Alice.Common.Utils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.BERT.Term
import Data.BERT.Parser

import qualified Data.ByteString.Char8 as B

data CommWorkerDirection = Read | Write

start :: CommWorkerDirection -> NR.NR -> B.ByteString -> IO ThreadId
start Read nrQ bytes = forkIO $ do
    --print "Comm worker starts."
    selfQ <- atomically newTQueue
    case parseTerm $ B.unpack bytes of
        Right bert ->
          case readBERT bert :: Either String (String, [String]) of
            Right ("messages", []) -> return ()
            Right ("messages", msgs) -> do
              let ws = map words msgs
              --print $ show ws
              forms <- mapM (mapM (askRoot selfQ nrQ)) ws
              print $ "CommWorker: " ++ show forms
              sendN nrQ "as" $ FromComm forms
            Right val -> print $ "wrong term: " ++ show val
            Left readErr -> print $ "read error: " ++ readErr
        Left err -> print $ "error: " ++ show err
start Write nrQ bytes = forkIO $ do
    undefined

askRoot :: TQueue Form -> NR.NR -> String -> IO Form
askRoot selfQ nrQ word = do
  --print "sending"
  sendN nrQ "root" $ WordToForm word selfQ
  --print "receiving"
  atomically $ readTQueue selfQ
