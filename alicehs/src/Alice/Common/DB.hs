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

import           Control.Monad

import           Alice.Common.Rules

import           Alice.Data.Assoc
import           Alice.Data.Emo
import           Alice.Data.Form
import           Alice.Data.GetAssocsResult

import           Database.HDBC
import           Database.HDBC.Sqlite3

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
  run conn ("CREATE TABLE IF NOT EXISTS assocs " ++ "(key INTEGER, first INTEGER, second INTEGER, strength INTEGER)") []
  -- Emo table. Stores emo values of forms.
  run
    conn
    ("CREATE TABLE IF NOT EXISTS emos (form INTEGER, ad INTEGER, do INTEGER, ox INTEGER, " ++
     "ph INTEGER, co INTEGER, en INTEGER)")
    []
  commit conn
  return conn

getLastForm :: AliceDB -> IO Form
getLastForm conn = do
  list <- quickQuery' conn "SELECT value FROM config WHERE key = (?)" [toSql "lastForm"]
  print $ "getLastForm list: " ++ show list
  case list of
    [[last]] -> do
      let lastForm = (read $ fromSql last) :: Int
          newLast = lastForm + 1
      setLastForm conn newLast
      return newLast
    [] -> do
      setLastForm conn 1
      return 1

setLastForm :: AliceDB -> Form -> IO ()
setLastForm conn form = do
  res <- run conn "UPDATE config SET value = (?) WHERE key = (?)" [toSql $ show form, toSql "lastForm"]
  if res > 0
    then do
      print $ "setLastForm res: " ++ show res
      commit conn
    else do
      res1 <-
        run conn "INSERT OR REPLACE INTO config (key, value) VALUES ((?), (?))" [toSql "lastForm", toSql $ show form]
      print $ "setLastForm res1: " ++ show res1
      commit conn

--------------------------------------------------------------------------------
-- %% Word %%
--------------------------------------------------------------------------------
getFormByWord :: AliceDB -> String -> IO Form
getFormByWord conn word = do
  list <- quickQuery' conn "SELECT form FROM words WHERE word=(?)" [toSql word]
  print $ "getFormByWord list: " ++ show list
  case list of
    [[form]] -> return $ fromSql form
    []       -> newWord conn word

newWord :: AliceDB -> String -> IO Form
newWord conn word = do
  lastForm <- getLastForm conn
  print $ "newWord lastForm: " ++ show lastForm
  res <- run conn "INSERT INTO words (word, form) VALUES ((?), (?))" [toSql word, toSql lastForm]
  print $ "newWord res: " ++ show res
  case res of
    a
      | a >= 0 -> do
        commit conn
        return lastForm
    other -> error "DB: can't create new word"

--------------------------------------------------------------------------------
-- %% Assocs %%
--------------------------------------------------------------------------------
getAssoc :: AliceDB -> Form -> Form -> IO Form
getAssoc conn first second = do
  let query = "SELECT * FROM assocs WHERE first = (?) AND second = (?)"
  list <- quickQuery' conn query [toSql first, toSql second]
  case list of
    [] ->
      undefined
    other -> undefined

getAssocs :: AliceDB -> Form -> IO GetAssocsResult
getAssocs conn form = do
  list <-
    quickQuery'
      conn
      "SELECT DISTINCT * FROM assocs WHERE first = (?) OR second = (?)"
      [toSql form, toSql form]
  case list of
    [] ->
      undefined

setAssoc :: AliceDB -> Form -> Form -> IO Form
setAssoc conn first second = do
  let query = "INSERT INTO assocs (key, first, second, strength) VALUES ((?), (?), (?), (?))"
      strength = 1 :: Int
  key <- getLastForm conn
  res <- run conn query [toSql key, toSql first, toSql second, toSql strength]
  case res of
      a
        | a >= 0 -> do
          commit conn
          return key
      other -> error "DB: can't create new assoc"

--------------------------------------------------------------------------------
-- %% Emo %%
--------------------------------------------------------------------------------
getEmo :: AliceDB -> Form -> IO Emo
getEmo conn form = do
  list <- quickQuery' conn "SELECT ad, do, ox, ph, co, en FROM emos WHERE form = (?)" [toSql form]
  case list of
    [] -> do
      undefined
    [[ad, dop, ox, ph, co, en]] -> do
      return
        Emo
        { emo_Ad = fromSql ad
        , emo_Do = fromSql dop
        , emo_Ox = fromSql ox
        , emo_Ph = fromSql ph
        , emo_Co = fromSql co
        , emo_En = fromSql en
        }
