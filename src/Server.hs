{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Server (
  runServer
) where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import Text.Blaze.Html5.Attributes as A
--import Text.JSON.Generic hiding (JSON)
import Data.Aeson

--TYPES
data Expansion =
  Expansion{
    expansionID::Integer,
    expansionFrom::String,
    expansionTo::String
  }
  deriving (Eq, Show, Read)

instance ToJSON Expansion where
    toJSON (Expansion expansionID expansionFrom expansionTo) = object ["id" .= expansionID, "from" .= expansionFrom, "to" .= expansionFrom]

data Rewrite =
  Rewrite{
    rewriteID::Integer,
    rewriteFrom::String,
    rewriteTo::String
  }
  deriving (Eq, Show, Read)

instance ToJSON Rewrite where
    toJSON (Rewrite rewriteID rewriteFrom rewriteTo) = object ["id" .= rewriteID, "from" .= rewriteFrom, "to" .= rewriteTo]

data Library =
  Library{
    libraryID::Integer,
    description::String,
    premises::[String],
    conclusion::String
  }
  deriving (Eq, Show, Read)

instance ToJSON Library where
    toJSON (Library libraryID description premises conclusion) = object ["id" .= libraryID, "description" .= description, "premises" .= premises, "conclusion" .= conclusion]

data Problem =
  Problem{
    problemID::Integer,
    problemDescription::String,
    problemPremises::[String],
    problemConclusion::String
  }
  deriving (Eq, Show, Read)

instance ToJSON Problem where
    toJSON (Problem problemID description premises conclusion) = object ["id" .= problemID, "description" .= description, "premises" .= premises, "conclusion" .= conclusion]

type Homepage = H.Html

--ROUTES
type Api =
  "home" :> Get '[HTML] Homepage :<|>
  "expansions" :> Get '[JSON] [Expansion] :<|>
  "expansions" :> Post '[JSON] Expansion :<|>
  "rewrites" :> Get '[JSON] [Rewrite] :<|>
  "rewrites" :> Post '[JSON] Rewrite :<|>
  "library" :> Get '[JSON] [Library] :<|>
  "library" :> Post '[JSON] Library :<|>
  "problem" :> Get '[JSON] [Problem] :<|>
  "problem" :> Post '[JSON] Problem

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
mkApp = return $ serve itemApi server

server :: Server Api
server =
  getHomePage :<|>
  getExpansions :<|>
  postExpansion :<|>
  getRewrites :<|>
  postRewrite :<|>
  getLibrary :<|>
  postLibrary:<|>
  getProblem :<|>
  postProblem

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
                          H.input H.! A.id "problem" H.! name "problem" H.! type_ "text"
                          H.input H.! A.id "problem-button" H.! type_ "button"
                      H.h2 "Problem"
                      H.div H.! A.id "problem-spec" $ mempty
                      H.div H.! A.id "problem-expansion" $ mempty
                  H.div H.! A.id "equational-helpers" H.! class_ "section-title" $ do
                      H.div H.! A.id "expansions-container" H.! class_ "bordered-rightcol" $ do
                          H.div H.! class_ "title" $ H.h2 "Expansions"
                          H.form H.! A.id "expansions" H.! class_ "hidden" $ do
                              H.input H.! A.id "expansions" H.! name "expansions" H.! type_ "text"
                              H.input H.! A.id "expansions-button" H.! type_ "button"
                          H.div H.! A.id "expansions-spec" $ mempty
                      H.div H.! A.id "rewrites-container" H.! class_ "bordered-rightcol" $ do
                          H.div H.! class_ "title" $ H.h2 "Rewrites"
                          H.form H.! A.id "rewrites" H.! class_ "hidden" $ do
                              H.input H.! A.id "rewrites" H.! name "rewrites" H.! type_ "text"
                              H.input H.! A.id "rewrites-button" H.! type_ "button"
                          H.div H.! A.id "rewrites-spec" $ mempty
                      H.div H.! A.id "library-container" H.! class_ "bordered-rightcol" $ do
                          H.div H.! class_ "title" $ H.h2 "Library"
                          H.form H.! A.id "library" H.! class_ "hidden" $ do
                              H.input H.! A.id "library" H.! name "library" H.! type_ "text"
                              H.input H.! A.id "library-button" H.! type_ "button"
                          H.div H.! A.id "library-spec" $ mempty
              H.script "$(\":header\").click(function(event){\n     //alert($(event.currentTarget).siblings(\"form\").innerHTML);\n     //$(event.currentTarget).siblings(\"form\").toggleClass(\"hidden\");\n     $( \".bordered-rightcol form\" ).toggleClass(\"hidden\");\n   });\n\n $(document).on('click', '#problem-button', function(){ $.ajax({\n    url: \"/problem\",\n    data: $(\"#problem\").contents(),\n    success: function( result ) {\n  str = JSON.stringify(result);\n\n $( \"#problem-spec\" ).html(result[0].premises+\" | \"+result[0].conclusion);\n},\n    error: function( result ) {\n    str = JSON.stringify(result);\n    alert(str);\n    }})}); \n \n $(document).on('click', '#expansions-button', function(){ $.ajax({\n    url: \"/expansions\",\n    data: $(\"#expansions\").contents(),\n    success: function( result ) {\n  str = JSON.stringify(result);\n $( \"#expansions-spec\" ).html(result[0].from+\" | \"+result[0].to);\n},\n    error: function( result ) {\n    str = JSON.stringify(result);\n    alert(str);\n    }})}); \n \n $(document).on('click', '#rewrites-button', function(){ $.ajax({\n    url: \"/rewrites\",\n    data: $(\"#rewrites\").contents(),\n    success: function( result ) {\n  str = JSON.stringify(result);\n $( \"#rewrites-spec\" ).html(result[0].from+\" | \"+result[0].to);\n},\n    error: function( result ) {\n  str = JSON.stringify(result);\n    alert(str);\n    }})}); \n \n $(document).on('click', '#library-button', function(){ $.ajax({\n    url: \"/library\",\n    data: $(\"#library\").contents(),\n    success: function( result ) {\n  str = JSON.stringify(result);\n $( \"#library-spec\" ).html(result[0].premises+\" | \"+result[0].conclusion);\n},\n    error: function( result ) {\n   str = JSON.stringify(result);\n    alert(str);\n    }})});"


getExpansions :: Handler [Expansion]
getExpansions = return [Expansion 1 "sequencein(an,intersect(A,B))" "sequencein(an,A) & sequencein(an,B)"]

postExpansion :: Handler Expansion
postExpansion = return $ Expansion 1 "sequencein(an,intersect(A,B))" "sequencein(an,A) & sequencein(an,B)"

getRewrites :: Handler [Rewrite]
getRewrites = return [Rewrite 1 "applyfn(compose(f,g),x)" "applyfn(f,applyfn(g,x))"]

postRewrite :: Handler Rewrite
postRewrite = return $ Rewrite 1 "applyfn(compose(f,g),x)" "applyfn(f,applyfn(g,x))"

getLibrary :: Handler [Library]
getLibrary = return [Library 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"]

postLibrary :: Handler Library
postLibrary = return $ Library 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"

getProblem :: Handler [Problem]
getProblem = return [Problem 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"]

postProblem :: Handler Problem
postProblem = return $ Problem 1 "" ["subsetof(A,B)", "subsetof(B,C)"] "subsetof(A,C)"
