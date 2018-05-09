module Database (
  connect,
  addExpansion,
  getExpansionRecords,
  getRewriteRecords,
  addRewrite,
  getLibraryRecords,
  addLibrary,
  getProblemRecords,
  addProblem
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Types
import Control.Monad(when)

connect :: FilePath -> IO Connection
connect fp =
  do dbh <- connectSqlite3 fp
     setupDB dbh
     return dbh

setupDB :: IConnection conn => conn -> IO ()
setupDB dbh =
  do tables <- getTables dbh
     when (not ("expansions" `elem` tables)) $
        do run dbh "CREATE TABLE expansions (\
                  \expansionID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                  \expansionFrom TEXT NOT NULL, \
                  \expansionTo TEXT NOT NULL)" []
           return ()
     when (not ("rewrites" `elem` tables)) $
        do run dbh "CREATE TABLE rewrites (\
                    \rewriteID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                    \rewriteFrom TEXT NOT NULL,\
                    \rewriteTo TEXT NOT NULL)" []
           return ()
     when (not ("library" `elem` tables)) $
        do run dbh "CREATE TABLE library (\
                    \libraryID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                    \libraryDescription TEXT NOT NULL, \
                    \librarypremises TEXT NOT NULL, \
                    \libraryConclusion TEXT NOT NULL)" []
           return ()
     when (not ("problem" `elem` tables)) $
        do run dbh "CREATE TABLE problem (\
                     \problemID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                     \description TEXT NOT NULL, \
                     \premises TEXT NOT NULL, \
                     \conclusion TEXT NOT NULL)" []
           return ()
     commit dbh

addExpansion :: IConnection conn => conn -> ExpansionRecord -> IO ExpansionRecord
addExpansion dbh expansion =
    handleSql errorHandler $
      do print "addExpansion"
         run dbh "INSERT INTO expansions (expansionFrom, expansionTo) VALUES (?, ?)"  [toSql (expansionFrom expansion), toSql (expansionTo expansion)]
         r <- quickQuery' dbh "SELECT expansionID FROM expansions WHERE expansionFrom = ?" [toSql (expansionFrom expansion)]
         case r of
           [[x]] -> return $ expansion {expansionId = fromSql x}
           y -> fail $ "addExpansion: unexpected result: " ++ show y
    where errorHandler e =
              do fail $ "Error adding expansion; does this expansion already exist?\n"
                     ++ show e

getExpansionRecords :: IConnection conn => conn -> IO [ExpansionRecord]
getExpansionRecords dbh = handleSql errorHandler $
                                do print "getExpansions"
                                   r <- quickQuery' dbh "SELECT * FROM expansions" []
                                   case r of
                                     [x] -> return $ ((map fromSql x)::[ExpansionRecord])
                                     y -> fail $ "getExpansions: unexpected result: " ++ show y
                              where errorHandler e =
                                        do fail $ "Error getting expansion:\n"
                                               ++ show e

getRewriteRecords :: IConnection conn => conn -> IO [RewriteRecord]
getRewriteRecords dbh = return [RewriteRecord 1 "applyfn(compose(f,g),x)" "applyfn(f,applyfn(g,x))"]

addRewrite :: IConnection conn => conn -> RewriteRecord -> IO RewriteRecord
addRewrite dbh rewrite = return $ RewriteRecord 1 "applyfn(compose(f,g),x)" "applyfn(f,applyfn(g,x))"

getLibraryRecords :: IConnection conn => conn -> IO [LibraryRecord]
getLibraryRecords dbh = return [LibraryRecord 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"]

addLibrary :: IConnection conn => conn -> LibraryRecord -> IO LibraryRecord
addLibrary dbh library = return $ LibraryRecord 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"

getProblemRecords :: IConnection conn => conn -> IO [ProblemRecord]
getProblemRecords dbh = return [ProblemRecord 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"]

addProblem :: IConnection conn => conn -> ProblemRecord -> IO ProblemRecord
addProblem dbh problem = return $ ProblemRecord 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"