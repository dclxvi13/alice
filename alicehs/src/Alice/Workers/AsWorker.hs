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
import Alice.Messages.ContextMsg

import qualified Alice.Data.Sentence as S
import Alice.Data.SForm
import Alice.Data.Form
import Alice.Data.GetAssocsResult
import Alice.Data.Emo
import Alice.Data.ContextUnit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Control.Monad

start :: TQueue RootMsg -> TQueue ContextMsg -> Emo -> [SForm] -> IO ThreadId
start rootQ contextQ currEmo sforms = forkIO $ do
    let flatten list = flatten' list []
        flatten' list acc =
            case list of
                [] -> acc
                x:xs -> flatten' xs $ acc ++ x
        sform2list sform =
            [(object, action)]
                    ++ (map (\(prop, _) -> (object, prop)) objectProps)
                    ++ (map (\(prop, _) -> (action, prop)) actionProps)
                    where
                        object = sform_Object sform
                        objectProps = sform_ObjectProperties sform
                        action = sform_Action sform
                        actionProps = sform_ActionProperties sform

        flatList = flatten $ map sform2list sforms

    forM_ flatList $ \pair -> do
            -- get assocs for pair
            assocs <- getAssocs rootQ pair

            -- get emo for pair
            emo <- getEmo rootQ pair currEmo

            -- send to context
            atomically $ writeTQueue contextQ $ AddContextUnit $ ContextUnit {
                contextUnit_pair = pair,
                contextUnit_emo = emo,
                contextUnit_assocs = assocs
            }

getAssocs :: TQueue RootMsg -> (Form, Form) -> IO GetAssocsResult
getAssocs rootQ pair = do
    selfQ <- atomically $ newTQueue

    atomically $ writeTQueue rootQ $ GetAssoc selfQ pair
    atomically $ readTQueue selfQ

getEmo :: TQueue RootMsg -> (Form, Form) -> Emo -> IO Emo
getEmo rootQ pair currEmo = do
    selfQ <- atomically $ newTQueue

    atomically $ writeTQueue rootQ $ GetEmo selfQ pair currEmo
    atomically $ readTQueue selfQ


