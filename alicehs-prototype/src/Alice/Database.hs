module Alice.Database where

import           Database.HDBC
import           Database.HDBC.Sqlite3

data AliceDB = AliceDB
  { aliceDBConnection :: Connection
  , addNewWord        :: Statement
  , selectFormByWord  :: Statement
  , getCountOfWords   :: Statement
  , addNewLink        :: Statement
  , selectLinkByForm  :: Statement
  , incrementWeight   :: Statement
  }

addNewWordStatement :: Connection -> IO Statement
addNewWordStatement conn = prepare conn "INSERT INTO words (word, form) VALUES (?, ?)"

selectFormByWordStatement :: Connection -> IO Statement
selectFormByWordStatement conn = prepare conn "SELECT form FROM words WHERE word=?"

getCountOfWordsStatement :: Connection -> IO Statement
getCountOfWordsStatement conn = prepare conn "SELECT count(word) FROM words WHERE word=?"

addNewLinkStatement :: Connection -> IO Statement
addNewLinkStatement conn =
  prepare conn "INSERT INTO links (startForm, endForm, weight) VALUES (?, ?, ?)"

selectLinkByFormStatement :: Connection -> IO Statement
selectLinkByFormStatement conn =
  prepare conn "SELECT * FROM links WHERE startForm=? ORDER BY weight DESC"

incrementWeightStatement :: Connection -> IO Statement
incrementWeightStatement conn =
  prepare conn "UPDATE links SET weight = weight + 1 WHERE startForm=? AND endForm=?"

connectAliceDB :: IO AliceDB
connectAliceDB = do
  conn <- connectSqlite3 "main.db"
  addNewWord <- addNewWordStatement conn
  selectFormByWord <- selectFormByWordStatement conn
  getCountOfWords <- getCountOfWordsStatement conn
  addNewLink <- addNewLinkStatement conn
  selectLinkByForm <- selectLinkByFormStatement conn
  incrementWeight <- incrementWeightStatement conn
  return $
    AliceDB
    { aliceDBConnection = conn
    , addNewWord = addNewWord
    , selectFormByWord = selectFormByWord
    , getCountOfWords = getCountOfWords
    , addNewLink = addNewLink
    , selectLinkByForm = selectLinkByForm
    , incrementWeight = incrementWeight
    }

executeStatement :: Statement -> [SqlValue] -> IO Integer
executeStatement = execute

selectAll :: Statement -> [SqlValue] -> IO [[SqlValue]]
selectAll statement values = execute statement values >> fetchAllRows' statement
