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

import qualified Alice.Data.Sentence as S
import Alice.Data.SForm
import Alice.Data.Form

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString.Char8 as B

data CommWorkerDirection = Read | Write

start :: CommWorkerDirection -> TQueue RootMsg -> TQueue AsMsg -> B.ByteString -> IO ThreadId
start Read rootQ asQ bytes = forkIO $ do
    selfQ <- atomically $ newTQueue
    let str = B.unpack bytes
    x <- S.decode str
    case x of
        Right [] -> return ()
        Right ss -> do
            sforms <- mapM (toSForm selfQ rootQ) ss
            atomically $ writeTQueue asQ $ FromComm sforms
        Left err -> print $ "error: " ++ err
start Write rootQ asQ bytes = forkIO $ do
    undefined

toSForm :: TQueue Form -> TQueue RootMsg -> S.Sentence -> IO SForm
toSForm selfQ rootQ (object, objectProperties, action, actionProperties) = do
    let ask = askRoot selfQ rootQ
    obj <- ask object
    act <- ask action
    objProps <- mapM (\(t, w) -> do
                        tform <- ask t
                        wform <- ask w
                        return (tform, wform)) objectProperties
    actProps <- mapM (\(t, w) -> do
                        tform <- ask t
                        wform <- ask w
                        return (tform, wform)) actionProperties
    return SForm { sform_Object = obj,
        sform_Action = act,
        sform_ObjectProperties = objProps,
        sform_ActionProperties = actProps}

askRoot :: TQueue Form -> TQueue RootMsg -> String -> IO Form
askRoot selfQ rootQ word = do
    atomically $ writeTQueue rootQ $ WordToForm word selfQ
    atomically $ readTQueue selfQ
