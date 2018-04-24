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
import Text.Blaze.Html5.Attributes
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

type Homepage = H.Html

--ROUTES
type Api =
  "home" :> Get '[HTML] Homepage :<|>
  "expansions" :> Get '[JSON] [Expansion] :<|>
  "expansions" :> Post '[JSON] Expansion :<|>
  "rewrites" :> Get '[JSON] [Rewrite] :<|>
  "rewrites" :> Post '[JSON] Rewrite :<|>
  "library" :> Get '[JSON] [Library] :<|>
  "library" :> Post '[JSON] Library

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
  postLibrary

--REST METHODS
getHomePage :: Handler Homepage
getHomePage = return home

home :: Homepage
home = H.docTypeHtml $ do
          H.head $ do
            H.title "MathJax TeX Test Page"
            H.script H.! type_ "text/x-mathjax-config" $ "MathJax.Hub.Config({ tex2jax: {inlineMath: [[\"$\",\"$\"],[\"\\\\(\",\"\\\\)\"]]}});"
            H.script H.! src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_HTML-full" $ ""
          H.body $ do
            H.p "When $a \\ne 0$, there are two solutions to \\(ax^2 + bx + c = 0\\) and they are $$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$"

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
