import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

connect :: FilePath -> IO Connection
connect fp =
  do dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
  do tables <- getTables dbh
    when (not ("expansions" `elem` tables)) $
      do run dbh "" []
          return ()
    when (not ("rewrites" `elem` tables)) $
        do run dbh "" []
          return ()
    when (not ("library" `elem` tables)) $
        do run dbh "" []
          return ()
    commit dbh

