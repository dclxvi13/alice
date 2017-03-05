-----------------------------------------------------------------------------
--
-- Module      :  Alice.Common.DB
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

module Alice.Common.DB where

import Control.Monad

import Alice.Common.Rules

import Alice.Data.Form
import Alice.Data.Assoc
import Alice.Data.GetAssocsResult
import Alice.Data.Emo

import Database.HDBC
import Database.HDBC.Sqlite3

type AliceDB = Connection

---------------------------------------
-- %% Common %%
---------------------------------------

connectDB :: IO AliceDB
connectDB = do
    conn <- connectSqlite3 "main.db"

    -- Config table.
    run conn "CREATE TABLE IF NOT EXISTS config (key VARCHAR(80), value VARCHAR(80))" []

    -- Words table. Associative array between words of human language and mind forms of Alice
    run conn "CREATE TABLE IF NOT EXISTS words (word VARCHAR(80), form INTEGER)" []

    -- Assocs table. Stores pair of mind forms linked together. Key is mind form associated with
    -- link. Index is strength of link.
    run conn ("CREATE TABLE IF NOT EXISTS assocs "
        ++ "(key INTEGER, first INTEGER, second INTEGER, index INTEGER)") []

    -- Emo table. Stores emo values of forms.
    run conn ("CREATE TABLE IF NOT EXISTS emos (form INTEGER, ad INTEGER, do INTEGER, ox INTEGER, "
        ++ "ph INTEGER, co INTEGER, en INTEGER)") []

    commit conn
    return conn

getLastForm :: AliceDB -> IO Form
getLastForm conn = do
    list <- quickQuery' conn "SELECT value FROM config WHERE key = (?)" [toSql "lastForm"]
    case list of
        [[last]] -> do
            let lastForm = (read $ fromSql last) :: Int
            setLastForm conn $ lastForm + 1
            return lastForm
        [[]] -> do
            setLastForm conn 1
            return 1

setLastForm :: AliceDB -> Form -> IO ()
setLastForm conn form = do
    run conn
        "INSERT OR REPLACE INTO config (key, value) VALUES ((?), (?))"
        [toSql "lastForm", toSql form]
    commit conn

--------------------------------------------------------------------------------
-- %% Word %%
--------------------------------------------------------------------------------

getFormByWord :: AliceDB -> String -> IO Form
getFormByWord conn word = do
    list <- quickQuery' conn "SELECT form FROM words WHERE word=(?)" [toSql word]
    case list of
        [[form]] -> return $ fromSql form
        [[]] -> newWord conn word

newWord :: AliceDB -> String -> IO Form
newWord conn word = do
    lastForm <- getLastForm conn
    run conn "INSERT INTO words (word, form) VALUES ((?), (?))" [toSql word, toSql lastForm]
    return lastForm

--------------------------------------------------------------------------------
-- %% Assocs %%
--------------------------------------------------------------------------------

getAssoc :: AliceDB -> Form -> Form -> IO GetAssocsResult
getAssoc conn first second = do
    list <- quickQuery' conn "SELECT index, key FROM assocs WHERE first = (?) AND second = (?)"
        [toSql first, toSql second]
    firstList <- quickQuery' conn "SELECT key, second, index FROM assocs WHERE first = (?)"
        [toSql first]
    secondList <- quickQuery' conn "SELECT key, first, index FROM assocs WHERE second = (?)"
        [toSql second]

    -- TODO gets list of linked form to existing
    firstList' <- forM firstList $ \[key, second, index] -> do
        undefined
    secondList' <- forM secondList $ \[key, first, index] -> do
        undefined

    case list of
        [[]] -> do
            key <- getLastForm conn
            run conn "INSERT INTO assocs (key, first, second, index) VALUES ((?), (?), (?), (?))"
                [toSql key, toSql first, toSql second, toSql (1::Int)]
            let mainAssoc = Assoc {
                    assoc_Key = key,
                    assoc_First = first,
                    assoc_Second = second,
                    assoc_Index = 1}

            return $
                GetAssocsResult {
                    getAssocsResult_Main = mainAssoc,
                    getAssocsResult_FirstResult = firstList',
                    getAssocsResult_SecondResult = secondList'
                }
        [[index', key']] -> do
            let index = (read $ fromSql index') :: Form
                key = (read $ fromSql key') :: Form
                mainAssoc = Assoc {
                    assoc_Key = key,
                    assoc_First = first,
                    assoc_Second = second,
                    assoc_Index = index}
            run conn "UPDATE assocs SET index = (?) WHERE first = (?) AND second = (?)"
                [toSql $ index + 1, toSql first, toSql second]

            return $
                GetAssocsResult {
                    getAssocsResult_Main = mainAssoc,
                    getAssocsResult_FirstResult = firstList',
                    getAssocsResult_SecondResult = secondList'
                }

--------------------------------------------------------------------------------
-- %% Emo %%
--------------------------------------------------------------------------------

getEmo :: AliceDB -> Form -> IO Emo
getEmo conn form = do
    list <- quickQuery' conn "SELECT ad, do, ox, ph, co, en FROM emos WHERE form = (?)"
        [toSql form]
    case list of
        [[]] -> do
            undefined
        [[ad, dop, ox, ph, co, en]] -> do
            return $ Emo {
                emo_Ad = fromSql $ ad,
                emo_Do = fromSql $ dop,
                emo_Ox = fromSql $ ox,
                emo_Ph = fromSql $ ph,
                emo_Co = fromSql $ co,
                emo_En = fromSql $ en}


