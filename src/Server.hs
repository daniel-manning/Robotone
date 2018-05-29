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


type Homepage = H.Html

--ROUTES
type Api =
  "home" :> Get '[HTML] Homepage :<|>
  "expansions" :> Get '[JSON] [ExpansionRecord] :<|>
  "expansions" :> ReqBody '[JSON] ExpansionRecord :> Post '[JSON] ExpansionRecord :<|>
  "rewrites" :> Get '[JSON] [RewriteRecord] :<|>
  "rewrites" :> ReqBody '[JSON] RewriteRecord :> Post '[JSON] RewriteRecord :<|>
  "library" :> Get '[JSON] [LibraryRecord] :<|>
  "library" :> ReqBody '[JSON] LibraryRecord :> Post '[JSON] LibraryRecord :<|>
  "problem" :> Get '[JSON] [ProblemRecord] :<|>
  "problem" :> ReqBody '[JSON] ProblemRecord :> Post '[JSON] ProblemRecord

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
  getHomePage :<|>
  getExpansions :<|>
  postExpansion :<|>
  getRewrites :<|>
  postRewrite :<|>
  getLibrary :<|>
  postLibrary :<|>
  getProblem :<|>
  postProblem
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

--REST METHODS
getHomePage :: Handler Homepage
getHomePage = return home

home :: Homepage
home = H.docTypeHtml $ do
          H.head $ do
              H.title "Proof Workspace"
              H.meta H.! httpEquiv "Content-Type" H.! content "text/html; charset=UTF-8"
              H.meta H.! httpEquiv "X-UA-Compatible" H.! content "IE=edge"
              H.meta H.! name "viewport" H.! content "width=device-width, initial-scale=1"
              H.style H.! type_ "text/css" $ ".minwidth { min-width:100px; width: auto !important; width: 100px; }\n   .hidden { visibility: hidden; display:none; }\n   #workspace { float:left; width:69%; }\n   #equational-helpers {float: right; width:29%;}\n   .bordered { border-style: solid ridge; border-width:thin;}  \n   .bordered-rightcol { border-style: solid ridge; border-width:thin; margin-top:10px;}\n   .section-title { text-align:center; }\n   div.section-title div.title {background-color: #B360C3;}"
              H.script H.! src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML-full" $ mempty
              H.script H.! type_ "text/javascript" H.! src "https://code.jquery.com/jquery-3.3.1.min.js" $ "<script type=\"text/x-mathjax-config\">\n  MathJax.Hub.Config({\n    extensions: [\"tex2jax.js\"],\n    jax: [\"input/TeX\",\"output/HTML-CSS\"],\n    tex2jax: {inlineMath: [[\"$\",\"$\"],[\"\\\\(\",\"\\\\)\"]]}\n  });"
          H.body $ do
              H.div H.! A.id "tabletop" H.! class_ "minwidth" $ do
                  H.div H.! A.id "workspace" H.! class_ "bordered" $ do
                      H.form H.! A.class_ "problem" $ do
                          H.label H.! for "problem-description" $ "Description:"
                          H.input H.! A.id "problem-description" H.! name "problem-description" H.! type_ "text"
                          H.label H.! for "problem-premises" $ "Premises:"
                          H.input H.! A.id "problem-premises" H.! name "problem-premises" H.! type_ "text"
                          H.label H.! for "problem-conclusion" $ "Conclusion:"
                          H.input H.! A.id "problem-conclusion" H.! name "problem-conclusion" H.! type_ "text"
                          H.input H.! A.id "problem-button" H.! type_ "button"
                      H.h2 "Problem"
                      H.div H.! A.id "problem-spec" $ mempty
                      H.div H.! A.id "problem-expansion" $ mempty
                  H.div H.! A.id "equational-helpers" H.! class_ "section-title" $ do
                      H.div H.! A.id "expansions-container" H.! class_ "bordered-rightcol" $ do
                          H.div H.! class_ "title" $ H.h2 "Expansions"
                          H.form H.! A.id "expansions" H.! class_ "hidden" $ do
                              H.label H.! for "expansionFrom" $ "From:"
                              H.input H.! A.id "expansionFrom" H.! name "expansionFrom" H.! type_ "text"
                              H.label H.! for "expansionTo" $ "To:"
                              H.input H.! A.id "expansionTo" H.! name "expansionTo" H.! type_ "text"
                              H.input H.! A.id "expansions-button" H.! type_ "button"
                          H.div H.! A.id "expansions-spec" $ mempty
                      H.div H.! A.id "rewrites-container" H.! class_ "bordered-rightcol" $ do
                          H.div H.! class_ "title" $ H.h2 "Rewrites"
                          H.form H.! A.id "rewrites" H.! class_ "hidden" $ do
                              H.label H.! for "rewriteFrom" $ "From:"
                              H.input H.! A.id "rewriteFrom" H.! name "rewriteFrom" H.! type_ "text"
                              H.label H.! for "rewriteTo" $ "To:"
                              H.input H.! A.id "rewriteTo" H.! name "rewriteTo" H.! type_ "text"
                              H.input H.! A.id "rewrites-button" H.! type_ "button"
                          H.div H.! A.id "rewrites-spec" $ mempty
                      H.div H.! A.id "library-container" H.! class_ "bordered-rightcol" $ do
                          H.div H.! class_ "title" $ H.h2 "Library"
                          H.form H.! A.id "library" H.! class_ "hidden" $ do
                              H.label H.! for "libraryDescription" $ "Description:"
                              H.input H.! A.id "libraryDescription" H.! name "libraryDescription" H.! type_ "text"
                              H.label H.! for "libraryPremises" $ "Premises:"
                              H.input H.! A.id "libraryPremises" H.! name "libraryPremises" H.! type_ "text"
                              H.label H.! for "libraryConclusion" $ "Conclusion:"
                              H.input H.! A.id "libraryConclusion" H.! name "libraryConclusion" H.! type_ "text"
                              H.input H.! A.id "library-button" H.! type_ "button"
                          H.div H.! A.id "library-spec" $ mempty
              H.script "$(\":header\").click(function(event){\n     //alert($(event.currentTarget).siblings(\"form\").innerHTML);\n     //$(event.currentTarget).siblings(\"form\").toggleClass(\"hidden\");\n     $( \".bordered-rightcol form\" ).toggleClass(\"hidden\");\n   });\n\n $(document).on('click', '#problem-button', function(){ $.ajax({\n    url: \"/problem\",\n    data: $(\"#problem\").contents(),\n    success: function( result ) {\n  str = JSON.stringify(result);\n\n $( \"#problem-spec\" ).html(result[0].premises+\" | \"+result[0].conclusion);\n},\n    error: function( result ) {\n    str = JSON.stringify(result);\n    alert(str);\n    }})}); \n \n $(document).on('click', '#expansions-button', function(){ alert(createJsonObject(\"#expansions\")); $.ajax({\n   headers: {\n 'Accept': 'application/json',\n 'Content-Type': 'application/json'\n}, type: \"POST\",\n url: \"/expansions\",\n    data: createJsonObject(\"#expansions\"),\n dataType: \"json\",\n success: function( result ) {\n  $( \"#expansions-spec\" ).html(result.expansionFrom+\" | \"+result.expansionTo);\n},\n    error: function( result ) {\n    str = JSON.stringify(result);\n    alert('error: ' + str);\n    }})}); \n \n $(document).on('click', '#rewrites-button', function(){ $.ajax({\n headers: {\n 'Accept': 'application/json',\n 'Content-Type': 'application/json'\n}, url: \"/rewrites\",\n type: \"POST\",\n   data: createJsonObject(\"#rewrites\"),\n dataType: \"json\",\n    success: function( result ) {\n  console.log(result); $( \"#rewrites-spec\" ).html(result.rewriteFrom+\" | \"+result.rewriteTo);\n},\n    error: function( result ) {\n  str = JSON.stringify(result);\n    alert(str);\n    }})}); \n \n $(document).on('click', '#library-button', function(){ $.ajax({\n  headers: {\n 'Accept': 'application/json',\n 'Content-Type': 'application/json'\n}, type: \"POST\",\n url: \"/library\",\n    data: createJsonObjectWithArray(\"#library\"),\n dataType: \"json\",\n   success: function( result ) {\n console.log(result); $( \"#library-spec\" ).html(result.premises+\" | \"+result.conclusion);\n},\n    error: function( result ) {\n   str = JSON.stringify(result);\n    alert(str);\n    }})});\n function createJsonObject(formName){\n var arr = $(formName).serializeArray();\n var json = {};\n jQuery.each(arr, function() {\n json[this.name] = this.value || '';\n });\n var json_string = JSON.stringify(json);\n json_string.replace(/(\\s*?{\\s*?|\\s*?,\\s*?)(['\"])?([a-zA-Z0-9]+)(['\"])?:/g, '$1\"$3\":');\n return json_string;\n } \n function createJsonObjectWithArray(formName){\n var arr = $(formName).serializeArray();\n var json = {};\n jQuery.each(arr, function() {\n json[this.name] = this.value || '';\n });\n var json_string = JSON.stringify(json);\n json_string.replace(/(\\s*?{\\s*?|\\s*?,\\s*?)(['\"])?([a-zA-Z0-9]+)(['\"])?:/g, '$1\"$3\":');\n eval('var jsonO = new Object(' + json_string + ')'); jsonO.libraryPremises = jsonO.libraryPremises.split(\"@\"); \n var json_edit_string = JSON.stringify(jsonO);\n return json_edit_string;\n }"



