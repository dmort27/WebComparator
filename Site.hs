module Site where

import Database.HDBC
import Database.HDBC.Sqlite3

sqliteFile = "/tmp/Tangkhul3.db"

connectDB = connectSqlite3 sqliteFile
