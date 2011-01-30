module Site where

import Database.HDBC
import Database.HDBC.Sqlite3

sqliteFile = "/var/webcomparator/WebCompBorderlands.sqlite"

connectDB = connectSqlite3 sqliteFile
