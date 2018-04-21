module Alice.Database where

import           Database.HDBC
import           Database.HDBC.Sqlite3

type AliceDB = Connection

addNewWordStatement :: AliceDB -> IO Statement
selectFormByWordStatement :: AliceDB -> IO Statement
getCountOfWordsStatement :: AliceDB -> IO Statement
addNewLinkStatement :: AliceDB -> IO Statement
selectLinkByFormStatement :: AliceDB -> IO Statement
incrementWeightStatement :: AliceDB -> IO Statement
addNewWordStatement conn = prepare conn "INSERT INTO words (word, form) VALUES (?, ?)"

selectFormByWordStatement conn = prepare conn "SELECT form FROM words WHERE word=?"

getCountOfWordsStatement conn = prepare conn "SELECT count(word) FROM words WHERE word=?"

addNewLinkStatement conn = prepare conn "INSERT INTO links (startForm, endForm, weight) VALUES (?, ?, ?)"

selectLinkByFormStatement conn = prepare conn "SELECT * FROM links WHERE startForm=? ORDER BY weight DESC"

incrementWeightStatement conn = prepare conn "UPDATE links SET weight = weight + 1 WHERE startForm=? AND endForm=?"

connectAliceDB :: IO AliceDB
connectAliceDB = connectSqlite3 "main.db"

executeStatement :: Statement -> [SqlValue] -> IO Integer
executeStatement = execute

selectAll :: Statement -> [SqlValue] -> IO [[SqlValue]]
selectAll statement values = execute statement values >> fetchAllRows' statement
