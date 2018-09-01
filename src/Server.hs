{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Server (
  runServer
) where

import Database
import Database.HDBC
import Database.HDBC.Sqlite3
--import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import Text.Blaze.Html5.Attributes as A
--import Text.JSON.Generic hiding (JSON)
import Data.Aeson
import GHC.Generics
import Types
import Data.Maybe

import RobotM
import Parser
import TestData
import Move

--
import DeletionMoves
import TidyingMoves
import ApplyingMoves
import Suspension
import Html.Tex
import Html.TexBase
import Html.Writeup
import WriteupBase

type Homepage = H.Html

--ROUTES
type Api =
  {-"home" :> Get '[HTML] Homepage :<|>-}
  "expansions" :> Get '[JSON] [ExpansionRecord] :<|>
  "expansions" :> ReqBody '[JSON] ExpansionRecord :> Post '[JSON] ExpansionRecord :<|>
  "rewrites" :> Get '[JSON] [RewriteRecord] :<|>
  "rewrites" :> ReqBody '[JSON] RewriteRecord :> Post '[JSON] RewriteRecord :<|>
  "library" :> Get '[JSON] [LibraryRecord] :<|>
  "library" :> ReqBody '[JSON] LibraryRecord :> Post '[JSON] LibraryRecord :<|>
  "problem" :> Get '[JSON] [ProblemRecord] :<|>
  "problem" :> ReqBody '[JSON] ProblemRecord :> Post '[JSON] ProblemRecord :<|>
{-  "solutionStep" :> Get '[JSON] [ProblemStep] :<|>
  "solutionStep" :> ReqBody '[JSON] ProblemStep :> Post '[JSON] ProblemStep :<|>-}
  "createInitialTableau" :> Get '[JSON] SolutionRecord :<|> -- :> ReqBody '[JSON] ProblemSetup :>
  Raw


itemApi :: Proxy Api
itemApi = Proxy

runServer:: IO ()
runServer = do
 let port = 3000
     settings =
       setPort port $
       setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
       defaultSettings
 runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
    dbh <- connect "maths.db"
    return $ serve itemApi $ server dbh

server :: IConnection conn => conn -> Server Api
server dbh =
  {-getHomePage :<|>-}
  getExpansions :<|>
  postExpansion :<|>
  getRewrites :<|>
  postRewrite :<|>
  getLibrary :<|>
  postLibrary :<|>
  getProblem :<|>
  postProblem :<|>
  {-getSolutionStep :<|>
  postSolutionStep :<|>-}
  createInitialTableau :<|>
  staticServer
  where
    getExpansions :: Handler [ExpansionRecord]
    getExpansions = liftIO $ getExpansionRecords dbh

    postExpansion :: ExpansionRecord -> Handler ExpansionRecord
    postExpansion expansion = liftIO $ addExpansion dbh expansion

    getRewrites :: Handler [RewriteRecord]
    getRewrites = liftIO $ getRewriteRecords dbh

    postRewrite ::RewriteRecord -> Handler RewriteRecord
    postRewrite rewrite = liftIO $ addRewrite dbh rewrite

    getLibrary :: Handler [LibraryRecord]
    getLibrary = liftIO $ getLibraryRecords dbh

    postLibrary :: LibraryRecord -> Handler LibraryRecord
    postLibrary library = liftIO $ addLibrary dbh library

    getProblem :: Handler [ProblemRecord]
    getProblem = liftIO $ getProblemRecords dbh

    postProblem :: ProblemRecord -> Handler ProblemRecord
    postProblem problem = liftIO $ addProblem dbh problem

    {-getSolutionStep :: Handler [ProblemStep]
    getSolutionStep = liftIO $ getProblemRecords dbh

    postSolutionStep :: ProblemStep -> Handler ProblemStep
    postSolutionStep problem = liftIO $ addProblem dbh problem-}

    createInitialTableau :: Handler SolutionRecord
    createInitialTableau = return(printSolution 100 (Problem "If $f$ is a continuous function and $(a_n) \to a$, then $(f(a_n)) \to f(a)$" ["continuous(f)","tendsto(an,a)"] "tendsto(applyfnpointwise(f,an),applyfn(f,a))"))

    staticServer :: ServerT Raw m
    staticServer = serveDirectoryWebApp "static-files"


    --REMOVE WHEN DONE
    pd = TestData.printingData
    lib = TestData.library

    movesFrom :: RobotState -> Tableau -> [(MoveDescription, Tableau)]
    movesFrom s t = case mapMaybe (runRobotM pd lib s) (allMovesByPriority t) of
                [] -> []
                (move@(MoveDescription _ _ _, t'), s'):_ -> move:movesFrom s' t'

    moveTypesByPriority :: [MoveType]
    moveTypesByPriority = [
       --Deletion
         deleteDone,
         deleteDoneDisjunct,
     --    deleteRedundantHypothesis, --move 1
         deleteDangling, --move 2
         deleteUnmatchable, -- move 3
       --Tidying (4-9)
         peelAndSplitUniversalConditionalTarget, --move 4
     --    flipNegativeTarget, --move 5
     --    flipNegativeHypothesis, --move 6
         splitDisjunctiveHypothesis, --move 7
     --    splitConjunctiveTarget, --move 8
         splitDisjunctiveTarget,
         peelBareUniversalTarget, -- move 9
         removeTarget, --move 10
         collapseSubtableauTarget,
       --Applying
         forwardsReasoning, --move 11
         forwardsLibraryReasoning, --move 11
         expandPreExistentialHypothesis, --move 13
         elementaryExpansionOfHypothesis, --move 12
         backwardsReasoning, --move 14
         backwardsLibraryReasoning, --move 14
         elementaryExpansionOfTarget, --move 15
         expandPreUniversalTarget, --move 16
         solveBullets,
         automaticRewrite,
       --Suspension
         unlockExistentialUniversalConditionalTarget, --move 17
         unlockExistentialTarget,
         expandPreExistentialTarget,
         convertDiamondToBullet,
         rewriteVariableVariableEquality,
         rewriteVariableTermEquality
       ]

    allMovesByPriority :: Tableau -> [RobotM (MoveDescription, Tableau)]
    allMovesByPriority  t = [f t | (MoveType f) <- moveTypesByPriority]

    printMove :: Int -> Tableau -> MoveDescription -> String
    printMove n oldTableau (MoveDescription statementsUsed clauses text) =
       unlines ["<div class=\"tableau\">" ++ (fit $ texTableauBolding pd statementsUsed oldTableau) ++ "</div>",
        "<div class=\"something-else\">" ++ show n ++ ". " ++ text ++ "</div>",
        "<div class=\"writeup-step\">" ++ (unwords (asSentence . writeup pd <$> clauses)) ++ "</div>"]

    printSolution :: Int -> Problem -> SolutionRecord
    printSolution max p =
      let
          hs = ["continuous(f)", "tendsto(an,a)"]
          t = "tendsto(applyfnpointwise(f,an),applyfn(f,a))"
          pd = TestData.printingData
          lib = TestData.library
          initialTableauM = createTableau False (parse formula <$> hs) $ parse formula t
          Just(initialTableau, s) = runRobotM pd lib initialRobotState initialTableauM
          moves = movesFrom s initialTableau
          (moveDescriptions, outputTableaux) = unzip moves
          proof = compress . eliminate . fuse $ concat [cs | MoveDescription _ cs _ <- moveDescriptions]

      in  if null moves
            then SolutionRecord { writeupOfMoves = "", tableauxSteps = [(fit $ tex pd initialTableau), "No moves possible."]}
            else let {
                    writeupOfMoves = if (not $ lengthAtLeast (max+1) moves) then (unwords $ asSentence . writeup pd <$> proof) else "";
                    steps = "\\begin{steps}";
                    tableauxSteps = zipWith3 printMove [1..max] (initialTableau:outputTableaux) moveDescriptions;
                    output = unlines [fit . tex pd . last . take max $ outputTableaux, ""];
                    nextS = if (lengthAtLeast (max+1) moves) then show max ++ " moves made; stopping as the Robot may be in an infinite loop." else case last moves of {(_, Done _) -> "\t" ++ show (length moves) ++ " moves made; problem solved. Problem solved.";  _ -> "\t" ++ show (length moves) ++ " moves made. No moves possible."};
                    double = "\\cleardoublepage\n";
                    endSteps = "\\end{steps}"
                } in (SolutionRecord { writeupOfMoves = writeupOfMoves, tableauxSteps = tableauxSteps })


lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast 0 _      = True
lengthAtLeast _ []     = False
lengthAtLeast n (_:xs) = lengthAtLeast (n-1) xs

     -------------------------
