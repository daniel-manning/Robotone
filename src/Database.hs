module Database (
  connect,
  addExpansion,
  getExpansionRecords,
  getRewriteRecords,
  addRewrite,
  getLibraryRecords,
  addLibrary,
  getProblemRecords,
  getProblemRecord,
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
                  \expansionFrom TEXT NOT NULL PRIMARY KEY, \
                  \expansionTo TEXT NOT NULL)" []
           return ()
     when (not ("rewrites" `elem` tables)) $
        do run dbh "CREATE TABLE rewrites (\
                    \rewriteFrom TEXT NOT NULL PRIMARY KEY,\
                    \rewriteTo TEXT NOT NULL)" []
           return ()
     when (not ("library" `elem` tables)) $
        do run dbh "CREATE TABLE library (\
                    \libraryDescription TEXT NOT NULL, \
                    \librarypremises TEXT PRIMARY KEY NOT NULL, \
                    \libraryConclusion TEXT NOT NULL)" []
           return ()
     when (not ("problem" `elem` tables)) $
        do run dbh "CREATE TABLE problem (\
                     \id INTEGER PRIMARY KEY AUTOINCREMENT,\
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
         r <- quickQuery' dbh "SELECT * FROM expansions WHERE expansionFrom = ?" [toSql (expansionFrom expansion)]
         return $ head $ map retrieveExpansion r
    where errorHandler e =
              do fail $ "Error adding expansion; does this expansion already exist?\n"
                     ++ show e
          retrieveExpansion [] = ExpansionRecord{ expansionFrom = "em", expansionTo = "em"}
          retrieveExpansion [sqlFrom, sqlTo] = ExpansionRecord{ expansionFrom = fromSql sqlFrom, expansionTo = fromSql sqlTo}
          retrieveExpansion x = ExpansionRecord{ expansionFrom = "ex", expansionTo = "ex"}

getExpansionRecords :: IConnection conn => conn -> IO [ExpansionRecord]
getExpansionRecords dbh = handleSql errorHandler $
                                do print "getExpansions"
                                   r <- quickQuery' dbh "SELECT * FROM expansions" []
                                   return $ map retrieveExpansion r
                                where errorHandler e =
                                          do fail $ "Error getting expansion:\n"
                                                 ++ show e
                                      retrieveExpansion [] = ExpansionRecord{ expansionFrom = "", expansionTo = ""}
                                      retrieveExpansion [sqlFrom, sqlTo] = ExpansionRecord{ expansionFrom = fromSql sqlFrom, expansionTo = fromSql sqlTo}
                                      retrieveExpansion x = ExpansionRecord{ expansionFrom = "", expansionTo = ""}

getRewriteRecords :: IConnection conn => conn -> IO [RewriteRecord]
getRewriteRecords dbh = handleSql errorHandler $
                              do print "getRewrites"
                                 r <- quickQuery' dbh "SELECT * FROM rewrites" []
                                 return $ map retrieveRewrite r
                              where errorHandler e =
                                        do fail $ "Error getting rewrite:\n"
                                             ++ show e
                                    retrieveRewrite [] = RewriteRecord{rewriteFrom = "", rewriteTo = ""}
                                    retrieveRewrite [sqlFrom, sqlTo] = RewriteRecord{rewriteFrom = fromSql sqlFrom, rewriteTo = fromSql sqlTo}
                                    retrieveRewrite x = RewriteRecord{rewriteFrom = "", rewriteTo = ""}

addRewrite :: IConnection conn => conn -> RewriteRecord -> IO RewriteRecord
addRewrite dbh rewrite = handleSql errorHandler $
                               do print "addRewrite"
                                  run dbh "INSERT INTO rewrites (rewriteFrom, rewriteTo) VALUES (?, ?)"  [toSql (rewriteFrom rewrite), toSql (rewriteTo rewrite)]
                                  r <- quickQuery' dbh "SELECT * FROM rewrites WHERE rewriteFrom = ?" [toSql (rewriteFrom rewrite)]
                                  return $ head $ map retrieveRewrite r
                             where errorHandler e =
                                       do fail $ "Error adding rewrite; does this rewrite already exist?\n"
                                              ++ show e
                                   retrieveRewrite [] = RewriteRecord{rewriteFrom = "", rewriteTo = ""}
                                   retrieveRewrite [sqlFrom, sqlTo] = RewriteRecord{rewriteFrom = fromSql sqlFrom, rewriteTo = fromSql sqlTo}
                                   retrieveRewrite x = RewriteRecord{rewriteFrom = "", rewriteTo = ""}

getLibraryRecords :: IConnection conn => conn -> IO [LibraryRecord]
getLibraryRecords dbh = handleSql errorHandler $
                              do print "getLibraryRecord"
                                 r <- quickQuery' dbh "SELECT * FROM library" []
                                 return $ map retrieveLibraryRecord r
                              where errorHandler e =
                                        do fail $ "Error getting rewrite:\n"
                                               ++ show e
                                    retrieveLibraryRecord [] = LibraryRecord{libraryDescription = "",
                                                                             libraryPremises = [""],
                                                                             libraryConclusion = ""}
                                    retrieveLibraryRecord [sqlDescription, sqlPremises, sqlConclusion] = LibraryRecord{libraryDescription = fromSql sqlDescription,
                                                                                                                       libraryPremises = extractList sqlPremises,
                                                                                                                       libraryConclusion = fromSql sqlConclusion}
                                    retrieveLibraryRecord x = LibraryRecord{libraryDescription = "",
                                                                             libraryPremises = [""],
                                                                             libraryConclusion = ""}
                                    extractList::SqlValue -> [String]
                                    extractList s = splitRegex (mkRegex "@") (fromSql s)

addLibrary :: IConnection conn => conn -> LibraryRecord -> IO LibraryRecord
addLibrary dbh library = handleSql errorHandler $
                               do print "addRewrite"
                                  run dbh "INSERT INTO library (libraryDescription, libraryPremises, libraryConclusion) VALUES (?, ?, ?)"  [toSql (libraryDescription library), toSql $ reduceList $ (libraryPremises library), toSql (libraryConclusion library)]
                                  r <- quickQuery' dbh "SELECT * FROM library WHERE libraryDescription = ?" [toSql (libraryDescription library)]
                                  return $ head $ map retrieveLibraryRecord r
                               where errorHandler e =
                                         do fail $ "Error adding library record; does this record already exist?\n"
                                             ++ show e
                                     reduceList sl = intercalate "@" sl
                                     retrieveLibraryRecord [] = LibraryRecord{libraryDescription = "",
                                                                             libraryPremises = [""],
                                                                             libraryConclusion = ""}
                                     retrieveLibraryRecord [sqlDescription, sqlPremises, sqlConclusion] = LibraryRecord{libraryDescription = fromSql sqlDescription,
                                                                                                                       libraryPremises = extractList sqlPremises,
                                                                                                                       libraryConclusion = fromSql sqlConclusion}
                                     retrieveLibraryRecord x = LibraryRecord{libraryDescription = "",
                                                                             libraryPremises = [""],
                                                                             libraryConclusion = ""}
                                     extractList::SqlValue -> [String]
                                     extractList s = splitRegex (mkRegex "@") (fromSql s)

getProblemRecords :: IConnection conn => conn -> IO [ProblemRecord]
getProblemRecords dbh =  handleSql errorHandler $
                               do print "getProblemRecord"
                                  r <- quickQuery' dbh "SELECT * FROM problem" []
                                  return $ map retrieveProblemRecord r
                               where errorHandler e =
                                          do fail $ "Error getting rewrite:\n"
                                                 ++ show e

                                     retrieveProblemRecord [] = ProblemRecord{problemID = Nothing, problemDescription = "", problemPremises = [""],  problemConclusion = ""}
                                     retrieveProblemRecord [sqlID, sqlDescription, sqlPremises, sqlConclusion] = ProblemRecord{
                                                                                                                        problemID = Just (fromSql sqlID),
                                                                                                                        problemDescription = fromSql sqlDescription,
                                                                                                                        problemPremises = extractList sqlPremises,
                                                                                                                        problemConclusion = fromSql sqlConclusion}
                                     retrieveProblemRecord x = ProblemRecord{problemID = Nothing, problemDescription = "", problemPremises = [""],  problemConclusion = ""}
                                     extractList::SqlValue -> [String]
                                     extractList s = splitRegex (mkRegex "@") (fromSql s)

getProblemRecord :: IConnection conn => Int -> conn -> IO ProblemRecord
getProblemRecord recordID dbh =  handleSql errorHandler $
                               do print "getProblemRecord"
                                  r <- quickQuery' dbh "SELECT * FROM problem where id = ?" [toSql recordID]
                                  return $ head $ map retrieveProblemRecord r
                               where errorHandler e =
                                          do fail $ "Error getting rewrite:\n"
                                                 ++ show e

                                     retrieveProblemRecord [] = ProblemRecord{problemID = Nothing, problemDescription = "", problemPremises = [""],  problemConclusion = ""}
                                     retrieveProblemRecord [sqlID, sqlDescription, sqlPremises, sqlConclusion] = ProblemRecord{
                                                                                                                        problemID = Just (fromSql sqlID),
                                                                                                                        problemDescription = fromSql sqlDescription,
                                                                                                                        problemPremises = extractList sqlPremises,
                                                                                                                        problemConclusion = fromSql sqlConclusion}
                                     retrieveProblemRecord x = ProblemRecord{problemID = Nothing, problemDescription = "", problemPremises = [""],  problemConclusion = ""}
                                     extractList::SqlValue -> [String]
                                     extractList s = splitRegex (mkRegex "@") (fromSql s)

addProblem :: IConnection conn => conn -> ProblemRecord -> IO ProblemRecord
addProblem dbh problem = handleSql errorHandler $
                               do print "addProblem"
                                  run dbh "INSERT INTO problem (description, premises, conclusion) VALUES (?, ?, ?)"  [toSql (problemDescription problem), toSql $ reduceList $ (problemPremises problem), toSql (problemConclusion problem)]
                                  r <- quickQuery' dbh "SELECT * FROM problem WHERE description = ?" [toSql (problemDescription problem)]
                                  return $ head $ map retrieveProblemRecord r
                               where errorHandler e =
                                          do fail $ "Error adding problem record; does this problem already exist?\n"
                                              ++ show e
                                     reduceList sl = intercalate "@" sl
                                     extractList::SqlValue -> [String]
                                     extractList s = splitRegex (mkRegex "@") (fromSql s)
                                     retrieveProblemRecord [] = ProblemRecord{problemID = Nothing, problemDescription = "", problemPremises = [""], problemConclusion = ""}
                                     retrieveProblemRecord [sqlID, sqlDescription, sqlPremises, sqlConclusion] = ProblemRecord{
                                                                                                                        problemID = Just (fromSql sqlID),
                                                                                                                        problemDescription = fromSql sqlDescription,
                                                                                                                        problemPremises = extractList sqlPremises,
                                                                                                                        problemConclusion = fromSql sqlConclusion}
                                     retrieveProblemRecord x = ProblemRecord{problemID = Nothing, problemDescription = "", problemPremises = [""], problemConclusion = ""}

