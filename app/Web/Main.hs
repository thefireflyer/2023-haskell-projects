-------------------------------------------------------------------------------
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Main where

import CMarkGFM (Node (..), commonmarkToNode, NodeType (..))
import Data.Maybe (fromMaybe)
import Data.Text (pack, stripPrefix, stripSuffix, unpack, Text)
import Lucid
import Turtle.Prelude qualified as Turtle
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, getCurrentTime)
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)
import Common
import qualified Control.Foldl
import Flow
import qualified Data.Text.Lazy
import Turtle (rmtree, fold, extension, mktree)
import Control.Monad (unless)

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

main :: [String] -> IO ()
main [] = main ["build"]
main ["build"] = build (posixSecondsToUTCTime 0)
main ["clean"] = rmtree outputPath
main ["help"] = help
main (x:_) = putError ("unknown command " ++ show x) *> help

-------------------------------------------------------------------------------

help :: IO ()
help = mapM_ putStrLn 
            ["Usage: test0 web [command]"
            ,"Commands:"
            ,"    build    Build site"
            ,"    clean    Delete all generated files"
            ,"    help     Display this help message"
            ,"Builds site if no command is given."]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

build :: UTCTime -> IO ()
build t0 = do
  t1 <- getCurrentTime
  files <- justPath . filterNew t0 <$> allFiles
  unless (null files) (putStrLn "Updating...")
  mapM_ buildPage files
  unless (null files) (putStrLn "Up to date!")
  threadDelay 1000
  if null files then build t0 else build t1

-------------------------------------------------------------------------------

allFiles :: IO [(String, UTCTime, Maybe String)]
allFiles = Turtle.fold
            ( do
                path <- Turtle.lstree inputPath
                time <- Turtle.datefile path
                return (path, time, extension path)
            )
            Control.Foldl.list

filterNew :: UTCTime -> [(String, UTCTime, Maybe String)] -> [(String, UTCTime, Maybe String)]
filterNew t0 = filter (\(_, t, e) -> t > t0 && e == Just "md")

justPath :: [(String, UTCTime, Maybe String)] -> [String]
justPath = map (\(p, _, _) -> p)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

buildPage :: String -> IO ()
buildPage path = do 
  putStrLn ("[Gen] " ++ path)
  src <- readFile path
  let loc = local2loc $ global2local path
  let final = loc2final loc
  mktree $ outputPath ++ loc
  let md = parseMd src 
           |> fixLinks
  html <- genHTML md
          |> renderText
          |> Data.Text.Lazy.unpack
          |> embedInTemplate
  writeFile final html

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

fixLinks :: Node -> Node
fixLinks (Node p (LINK url text) xs) = (Node p (LINK (fixLink url) text) (map fixLinks xs))
fixLinks (Node p (IMAGE url text) xs) = (Node p (IMAGE (fixLink url) text) (map fixLinks xs))
fixLinks (Node p t xs) = (Node p t (map fixLinks xs))

fixLink :: Text -> Text
fixLink = unpack .> local2loc .> loc2final .> pack

-------------------------------------------------------------------------------

global2local :: String -> String
global2local = unpack . fromMaybe (error "invalid path") . stripPrefix (pack inputPath) . pack

local2loc :: String -> String
local2loc = unpack . fromMaybe (error "invalid path") . stripSuffix (pack ".md") . pack

loc2final :: String -> String
loc2final s = outputPath ++ s ++ ".html"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

parseMd :: String -> Node
parseMd = commonmarkToNode [] [] . pack

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

genHTML :: Node -> Html ()
genHTML ast =
  body_
    ( do
        -- header_ (do 
        --           a_ [href_ $ pack $ baseUrl ++ "index.html" ] "thefireflyer"
        --           p_ ">>="
        --           a_ [href_ $ pack $ baseUrl ++ "..."] "..."
        --         )
        div_ [id_ "main"] (do div_ [id_ "page"] (nodeHtml ast))
        -- footer_
        --   ( a_ [href_ $ pack $ baseUrl ++ "index.html"] "✿ thefireflyer ✿"
        --   )
    )

-------------------------------------------------------------------------------

nodeHtml :: Node -> Html ()
nodeHtml (Node _ nT children) = 
  case nT of
    DOCUMENT        -> div_ [class_ "page-inner"] (mapM_ nodeHtml children)
    THEMATIC_BREAK  -> br_ []
    PARAGRAPH       -> p_ (mapM_ nodeHtml children)
    BLOCK_QUOTE     -> div_ [class_ "quote"] (mapM_ nodeHtml children)
    HTML_BLOCK x    -> toHtmlRaw (unpack x)
    HEADING 1       -> h1_ (mapM_ nodeHtml children)
    HEADING 2       -> h2_ (mapM_ nodeHtml children)
    HEADING 3       -> h3_ (mapM_ nodeHtml children)
    LIST _          -> ul_ (mapM_ nodeHtml children)
    ITEM            -> li_ (mapM_ nodeHtml children)
    HTML_INLINE x   -> toHtmlRaw (unpack x)
    EMPH            -> i_ (mapM_ nodeHtml children)
    STRONG          -> b_ (mapM_ nodeHtml children)
    LINK url _      -> a_ [href_ url] (mapM_ nodeHtml children)
    CODE_BLOCK info x -> 
      div_ [class_ "code-holder"]
      ( div_ [class_ "code"] (do p_ (toHtml (unpack info))
                                 pre_ (toHtml (unpack x)))
      )
    TEXT x -> toHtml (unpack x)
    _ -> mapM_ nodeHtml children

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

embedInTemplate :: String -> IO String
embedInTemplate inner = do
  template <- lines <$> readFile (inputPath ++ "/template/template.html")
  let (left, middle) = span (/= "%%body%%") template
  let (_, right) = span (== "%%body%%") middle
  let whole = (unlines left) ++ inner ++ (unlines right)
  pure whole

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- liftIO $ case parseMd src of
--   Left dat -> writeFile (outP path) (genHTML dat css js)
--   Right err -> print err