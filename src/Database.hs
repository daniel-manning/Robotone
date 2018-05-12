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
import Text.Regex
import Data.List

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
                                   return $ map retrieveExpansion r
                                where errorHandler e =
                                          do fail $ "Error getting expansion:\n"
                                                 ++ show e
                                      retrieveExpansion [sqlId, sqlFrom, sqlTo] = ExpansionRecord{expansionId = fromSql sqlId,
                                                                                                   expansionFrom = fromSql sqlFrom,
                                                                                                   expansionTo = fromSql sqlTo}

getRewriteRecords :: IConnection conn => conn -> IO [RewriteRecord]
getRewriteRecords dbh = handleSql errorHandler $
                              do print "getRewrites"
                                 r <- quickQuery' dbh "SELECT * FROM rewrites" []
                                 return $ map retrieveRewrite r
                              where errorHandler e =
                                        do fail $ "Error getting rewrite:\n"
                                             ++ show e
                                    retrieveRewrite [sqlId, sqlFrom, sqlTo] = RewriteRecord{rewriteId = fromSql sqlId,
                                                                                          rewriteFrom = fromSql sqlFrom,
                                                                                          rewriteTo = fromSql sqlTo}

addRewrite :: IConnection conn => conn -> RewriteRecord -> IO RewriteRecord
addRewrite dbh rewrite = handleSql errorHandler $
                               do print "addRewrite"
                                  run dbh "INSERT INTO rewrites (rewriteFrom, rewriteTo) VALUES (?, ?)"  [toSql (rewriteFrom rewrite), toSql (rewriteTo rewrite)]
                                  r <- quickQuery' dbh "SELECT rewriteID FROM rewrites WHERE rewriteFrom = ?" [toSql (rewriteFrom rewrite)]
                                  case r of
                                    [[x]] -> return $ rewrite {rewriteId = fromSql x}
                                    y -> fail $ "addRewrite: unexpected result: " ++ show y
                             where errorHandler e =
                                       do fail $ "Error adding rewrite; does this rewrite already exist?\n"
                                              ++ show e

getLibraryRecords :: IConnection conn => conn -> IO [LibraryRecord]
getLibraryRecords dbh = handleSql errorHandler $
                              do print "getLibraryRecord"
                                 r <- quickQuery' dbh "SELECT * FROM library" []
                                 return $ map retrieveLibraryRecord r
                              where errorHandler e =
                                        do fail $ "Error getting rewrite:\n"
                                               ++ show e
                                    retrieveLibraryRecord [sqlId, sqlDescription, sqlPremises, sqlConclusion] = LibraryRecord{libraryID = fromSql sqlId,
                                                                                                  libraryDescription = fromSql sqlDescription,
                                                                                                  libraryPremises = extractList sqlPremises,
                                                                                                  libraryConclusion = fromSql sqlConclusion}
                                    extractList::SqlValue -> [String]
                                    extractList s = splitRegex (mkRegex "@") (fromSql s)

addLibrary :: IConnection conn => conn -> LibraryRecord -> IO LibraryRecord
addLibrary dbh library = handleSql errorHandler $
                               do print "addRewrite"
                                  run dbh "INSERT INTO library (libraryDescription, libraryPremises, libraryConclusion) VALUES (?, ?, ?)"  [toSql (libraryDescription library), toSql $ reduceList $ (libraryPremises library), toSql (libraryConclusion library)]
                                  r <- quickQuery' dbh "SELECT libraryID FROM library WHERE libraryDescription = ?" [toSql (libraryDescription library)]
                                  case r of
                                    [[x]] -> return $ library {libraryID = fromSql x}
                                    y -> fail $ "addLibrary: unexpected result: " ++ show y
                               where errorHandler e =
                                         do fail $ "Error adding library record; does this record already exist?\n"
                                             ++ show e
                                     reduceList sl = intercalate "@" sl

getProblemRecords :: IConnection conn => conn -> IO [ProblemRecord]
getProblemRecords dbh =  handleSql errorHandler $
                               do print "getProblemRecord"
                                  r <- quickQuery' dbh "SELECT * FROM problem" []
                                  return $ map retrieveProblemRecord r
                               where errorHandler e =
                                          do fail $ "Error getting rewrite:\n"
                                                 ++ show e
                                     retrieveProblemRecord [sqlId, sqlDescription, sqlPremises, sqlConclusion] = ProblemRecord{problemID = fromSql sqlId,
                                                                                                    problemDescription = fromSql sqlDescription,
                                                                                                    problemPremises = extractList sqlPremises,
                                                                                                    problemConclusion = fromSql sqlConclusion}
                                     extractList::SqlValue -> [String]
                                     extractList s = splitRegex (mkRegex "@") (fromSql s)

addProblem :: IConnection conn => conn -> ProblemRecord -> IO ProblemRecord
addProblem dbh problem = handleSql errorHandler $
                               do print "addProblem"
                                  run dbh "INSERT INTO problem (description, premises, conclusion) VALUES (?, ?, ?)"  [toSql (problemDescription problem), toSql $ reduceList $ (problemPremises problem), toSql (problemConclusion problem)]
                                  r <- quickQuery' dbh "SELECT problemID FROM problem WHERE description = ?" [toSql (problemDescription problem)]
                                  case r of
                                    [[x]] -> return $ problem {problemID = fromSql x}
                                    y -> fail $ "addProblem: unexpected result: " ++ show y
                               where errorHandler e =
                                          do fail $ "Error adding problem record; does this problem already exist?\n"
                                              ++ show e
                                     reduceList sl = intercalate "@" sl
