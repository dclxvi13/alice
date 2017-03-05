-----------------------------------------------------------------------------
--
-- Module      :  Alice.Servers.ContextSrv
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

module Alice.Servers.ContextSrv (start) where

import Alice.Messages.RootMsg
import Alice.Messages.ContextMsg
import Alice.Messages.ConsMsg
import Alice.Messages.AsMsg

import Alice.Data.Emo
import Alice.Data.ContextUnit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

data ContextState = ContextState {
    contextState_selfQ :: TQueue ContextMsg,
    contextState_rootQ :: TQueue RootMsg,
    contextState_consQ :: TQueue ConsMsg,
    contextState_asQ :: TQueue AsMsg,

    contextState_context :: [ContextUnit],
    contextState_currentEmo :: Emo
}

start :: TQueue ContextMsg -> TQueue RootMsg -> TQueue ConsMsg -> TQueue AsMsg -> IO ThreadId
start selfQ rootQ consQ asQ = forkIO $ do
    state <- initSrv selfQ rootQ consQ asQ
    loop state

initSrv :: TQueue ContextMsg -> TQueue RootMsg -> TQueue ConsMsg -> TQueue AsMsg -> IO ContextState
initSrv selfQ rootQ consQ asQ = do

    return $ ContextState {
        contextState_selfQ = selfQ,
        contextState_rootQ = rootQ,
        contextState_consQ = consQ,
        contextState_asQ = asQ,

        contextState_context = [],
        contextState_currentEmo = Emo {
            emo_Ad = 0 ,
            emo_Co = 0 ,
            emo_Do = 0 ,
            emo_En = 0 ,
            emo_Ox = 0 ,
            emo_Ph = 0 }
    }

loop :: ContextState -> IO ()
loop state = do
    msg <- atomically $ readTQueue $ contextState_selfQ state
    case msg of
        AddContextUnit unit -> do
            let emo = calculateEmo (contextState_currentEmo state) (contextUnit_emo unit)
                contextLength = 1000

                context = contextState_context state
                context' = take contextLength $ unit:context

            -- TODO open bridge on massive change

            -- update current emo of AsSrv
            atomically $ writeTQueue (contextState_asQ state) $ UpdateCurrent emo

            loop $ state {contextState_currentEmo = emo, contextState_context = context'}

calculateEmo :: Emo -> Emo -> Emo
calculateEmo old new =
    --TODO
    undefined


