-------------------------------------------------------------------------------
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Main where

import CMarkGFM (Node, commonmarkToNode)
import Data.Maybe (fromMaybe)
import Data.Text (pack, stripPrefix, stripSuffix, unpack)
import Lucid
import Turtle (liftIO, mktree, rmtree, sh)
import Turtle.Pattern qualified as Turtle
import Turtle.Prelude qualified as Turtle

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

baseUrl :: String
baseUrl = outputPath -- temp

inputPath :: FilePath
inputPath = "/home/casey/dev/hsk/test0/site/"

outputPath :: FilePath
outputPath = "/home/casey/dev/hsk/test0/appdata/site/"

exclude :: [String]
exclude = []

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

type JS = String

type CSS = String

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main [] = main ["build"]
main ["build"] = genSt
main ["watch"] = error "todo"
main ["clear"] = rmtree outputPath
main _ = putStrLn "invalid args"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

genSt :: IO ()
genSt =
  sh $ do
    css <- liftIO $ readFile $ inputPath ++ "global.css"
    js <- liftIO $ readFile $ inputPath ++ "global.js"

    path <- Turtle.find (Turtle.ends $ Turtle.text $ pack ".md") inputPath
    liftIO $ putStrLn ("[Gen] " ++ path)
    src <- liftIO $ readFile path
    mktree (outD path)
    liftIO $ renderToFile (outP path) (genHTML (parseMd src) css js)
  where
    outD path =
      outputPath
        ++ unpack
          ( fromMaybe
              (error "invalid path")
              (stripSuffix (pack ".md") (fromMaybe (error "invalid path") (stripPrefix (pack inputPath) (pack path))))
          )
        ++ "/"
    outP path = outD path ++ "index.html"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

parseMd :: String -> Node
parseMd = commonmarkToNode [] [] . pack

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

genHTML :: Node -> CSS -> JS -> Html ()
genHTML ast css js =
  doctypehtml_ $ do
    head_
      ( do
          title_ pTitle
          meta_ [name_ "description", content_ pDesc]
          meta_ [name_ "viewport", content_ "width=device-width"]
          link_ [rel_ "icon", href_ pLogo]
      )
    body_
      ( do
          header_ ""
          div_ [class_ "main"] (do p_ "test")
          footer_
            ( a_ [href_ $ pack $ baseUrl ++ "index.html"] "✿ thefireflyer ✿"
            )
      )
    style_ (pack css)
    script_ (pack js)
  where
    pTitle = "thefireflyer"
    pDesc = "testing..."
    pLogo = "..."

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- liftIO $ case parseMd src of
--   Left dat -> writeFile (outP path) (genHTML dat css js)
--   Right err -> print err