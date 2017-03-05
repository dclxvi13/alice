-----------------------------------------------------------------------------
--
-- Module      :  Alice.Data.Sentence
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

module Alice.Data.Sentence where

import Prelude hiding (Word)

import qualified Text.JSON as JSON

import Data.Either

type Sentence = (Object, ObjectProperties, Action, ActionProperties)

type Object = String

type Action = String

type ObjectProperties = [Property]

type ActionProperties = [Property]

type Property = (Type, Word)

type Type = String

type Word = String


decode :: String -> IO (Either String [Sentence])
decode str =
    case JSON.decode str :: JSON.Result (JSON.JSObject [String]) of
        JSON.Ok obj -> do
            let val = JSON.fromJSObject obj
            case val of
                [("messages", list)] -> do
                    l <- mapM decodeSentence list

                    --print $ show l

                    let objs = rights l
                    if length objs == length list
                        then return $ Right objs
                        else return $ Left "error: can't decode sentence"
                other -> return $ Left $ show other
        JSON.Error err -> return $ Left $ err ++ " line 63"

decodeSentence :: String -> IO (Either String Sentence)
decodeSentence e =
    case JSON.decode e :: JSON.Result (JSON.JSObject JSON.JSValue) of
        JSON.Ok obj ->
            case JSON.valFromObj "object" obj :: JSON.Result String of
                JSON.Ok "" -> undefined
                JSON.Ok o ->
                    case JSON.valFromObj "action" obj :: JSON.Result String of
                        JSON.Ok "" -> undefined
                        JSON.Ok a ->
                            case JSON.valFromObj "objectProperties" obj
                                :: JSON.Result [JSON.JSObject JSON.JSValue] of
                                JSON.Ok op ->
                                    case JSON.valFromObj "actionProperties" obj
                                        :: JSON.Result [JSON.JSObject JSON.JSValue] of
                                        JSON.Ok ap -> do
                                            o1 <- decodeProps op
                                            case o1 of
                                                Left _ -> undefined
                                                Right objProps -> do
                                                    a1 <- decodeProps ap
                                                    case a1 of
                                                        Left _ -> undefined
                                                        Right actProps ->
                                                            return $ Right $ sentence o objProps a actProps
                                        JSON.Error _ -> undefined
                                JSON.Error _ -> undefined
                        JSON.Error _ -> undefined
                JSON.Error _ -> undefined
        JSON.Error err ->
            return $ Left $ err ++ " line 117 " ++ show e

decodeProps :: [JSON.JSObject JSON.JSValue] -> IO (Either String [(String, String)])
decodeProps list = do
    l <- mapM (\e ->
                    case JSON.valFromObj "word" e :: JSON.Result String of
                        JSON.Ok "" -> return $ Left "err: empty word"
                        JSON.Ok word ->
                            case JSON.valFromObj "type" e :: JSON.Result String of
                                JSON.Ok "" -> return $ Left "err: empty type"
                                JSON.Ok t -> return $ Right $ property t word
                                JSON.Error err1 -> return $ Left err1
                        JSON.Error err -> return $ Left err) list
    let props = rights l

    --print $ show l

    if length props == length list
        then return $ Right props
        else return $ Left "error: can't decode props"


property :: Type -> Word -> Property
property t w = (t, w)

--group w props = (w, props)

sentence :: Object -> ObjectProperties -> Action -> ActionProperties -> Sentence
sentence obj oprops act aprops = (obj, oprops, act, aprops)
