-------------------------------------------------------------------------------
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Web.Main where

import CMarkGFM (ListAttributes (ListAttributes), ListType (ORDERED_LIST), Node (..), NodeType (..), commonmarkToNode)
import Common
import Control.Concurrent (Chan, forkIO, newChan, readChan, threadDelay, writeChan)
import Control.Foldl qualified
import Control.Monad (filterM, forever, unless, when, (>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Reflection (Given, given)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import Flow
import Lucid
import Network.WebSockets qualified as WS
import System.Directory.OsPath qualified as Dir
import System.INotify (Event (..), EventVariety (Create, Delete, Modify, Move, MoveIn, MoveOut), addWatch, initINotify)
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OP
import Turtle (extension, fold, mktree)
import Turtle.Prelude qualified as Turtle
import Web.Types

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

baseUrl :: String
baseUrl = outputPath -- temp

inputPath :: FilePath
inputPath = "/home/casey/dev/2023-haskell-projects/site/"

outputPath :: FilePath
outputPath = "/home/casey/dev/hsk/test0/appdata/site/"

exclude :: [String]
exclude = []

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main [] = help
main ["dev"] = giveCfg dev
main ["build"] = build True (posixSecondsToUTCTime 0)
main ["clean"] = giveCfg (Dir.removeDirectoryRecursive (outPath given))
main ["help"] = help
main (x : _) = putError ("unknown command " ++ show x) *> help

-------------------------------------------------------------------------------

help :: IO ()
help =
  mapM_
    putStrLn
    [ "Usage: test0 web [command]",
      "Commands:",
      "    build    Build site",
      "    clean    Delete all generated files",
      "    help     Display this help message"
    ]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

dev :: (Given Cfg) => IO ()
dev = do
  build False (posixSecondsToUTCTime 0)

  ch <- newChan
  putStrLn $ "Listening on ws://" <> wsAddr given <> ":" <> show (wsPort given)
  void . forkIO $
    WS.runServer
      (wsAddr given)
      (wsPort given)
      (WS.acceptRequest >=> wsConn ch)

  inotify <- initINotify
  let es = [Modify, Move, MoveIn, MoveOut, Create, Delete]
  Dir.setCurrentDirectory (srcPath given)

  let onEvent :: OsPath -> Event -> IO ()
      onEvent dir (Modified False (Just path)) = onOutdated dir path
      onEvent dir (Created False path) = onOutdated dir path
      onEvent dir (MovedIn False path _) = onOutdated dir path
      onEvent dir (Created True path) = onNewFolder dir path
      onEvent dir (MovedIn True path _) = onNewFolder dir path
      onEvent _ a = print a

      onOutdated :: OsPath -> B.ByteString -> IO ()
      onOutdated d p = do
        path <- (d OP.</>) <$> (OP.encodeFS =<< B.toFilePath p)
        putStr $ "[*] " <> show path
        let (pid, ext) = OP.splitExtension path
        case ext of
          [osp|.md|] -> do
            putStrLn " (markdown)"
            html <- Dir.makeAbsolute path >>= OP.decodeFS >>= buildPage
            path' <- T.drop 2 . T.pack <$> OP.decodeFS (pid OP.<.> [osp|.html|])
            writeChan ch $ Mutate path' (T.pack html)
          [osp|.html|] -> do
            putStrLn " (html)"
            build False (posixSecondsToUTCTime 0)
            writeChan ch Refresh
          [osp|.js|] -> do
            putStrLn " (js)"
            writeChan ch Refresh
          [osp|.css|] -> do
            putStrLn " (css)"
            writeChan ch Refresh
          _ -> putStrLn ""

      onNewFolder :: OsPath -> ByteString -> IO ()
      onNewFolder dir path =
        B.toFilePath path
          >>= OP.encodeFS
          >>= recWatch . (dir OP.</>)

      recWatch :: OsPath -> IO ()
      recWatch dir = do
        putStrLn $ "Watching " <> show dir
        bdir <- OP.decodeFS dir >>= B.fromFilePath
        void $ addWatch inotify es bdir (onEvent dir)
        Dir.listDirectory dir
          >>= filterM Dir.doesDirectoryExist . map (dir OP.</>)
          >>= mapM_ recWatch

  recWatch [osp|.|]

  putStrLn "Hit enter to terminate."
  void getLine

wsConn :: Chan Action -> WS.Connection -> IO ()
wsConn ch conn =
  WS.withPingThread conn 10 (return ()) $ do
    page <- WS.receiveData conn :: IO Text
    putStrLn $ "\ESC[2mConnected to client (" <> show page <> ")\ESC[0m"
    forever $ do
      msg <- readChan ch
      -- putStrLn $ "\ESC[2m" <> show msg <> "\ESC[0m"
      serializeAction msg >>= WS.sendTextData conn

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

build :: Bool -> UTCTime -> IO ()
build w t0 = do
  t1 <- getCurrentTime
  files <- justPath . filterNew t0 <$> allFiles
  unless (null files) (putStrLn "Updating...")
  mapM_ buildPage files
  unless (null files) (putStrLn "Up to date!")
  threadDelay 1000
  when w $ if null files then build w t0 else build w t1

-------------------------------------------------------------------------------

allFiles :: IO [(String, UTCTime, Maybe String)]
allFiles =
  Turtle.fold
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

buildPage :: String -> IO String
buildPage path = do
  -- putStrLn ("[Gen] " ++ path)
  src <- readFile path
  let loc = local2loc $ global2local path
  let final = loc2final loc
  mktree $ outputPath ++ loc
  let md =
        parseMd src
          |> fixLinks
  html <-
    genHTML md
      |> renderText
      |> TL.unpack
      |> embedInTemplate
  writeFile final html
  return html

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- mapMd :: (Node -> NodeType)
-- mapMd f

fixLinks :: Node -> Node
fixLinks (Node p t xs) = Node p t' (map fixLinks xs)
  where
    t' = case t of
      (LINK url text) -> LINK (fixLink url) text
      (IMAGE url text) -> IMAGE (fixLink url) text
      _ -> t

fixLink :: Text -> Text
fixLink x = case T.head x of
  'h' | "http://" `T.isPrefixOf` x -> x
  'h' | "https://" `T.isPrefixOf` x -> x
  'w' | "ws://" `T.isPrefixOf` x -> x
  'w' | "wss://" `T.isPrefixOf` x -> x
  '/' -> T.pack baseUrl <> T.tail (fixExt x)
  _ -> fixExt x

fixExt :: Text -> Text
fixExt x = T.stripSuffix (T.pack ".md") x |> maybe x (<> T.pack ".html")

-------------------------------------------------------------------------------

global2local :: String -> String
global2local = T.unpack . fromMaybe (error "invalid path") . T.stripPrefix (T.pack inputPath) . T.pack

local2loc :: String -> String
local2loc = T.unpack . fromMaybe (error "invalid path") . T.stripSuffix (T.pack ".md") . T.pack

loc2final :: String -> String
loc2final s = outputPath ++ s ++ ".html"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

parseMd :: String -> Node
parseMd = commonmarkToNode [] [] . T.pack

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

genHTML :: Node -> Html ()
genHTML ast =
  body_
    ( do
        header_
          ( do
              a_ [href_ $ T.pack $ baseUrl ++ "index.html"] "thefireflyer"
              p_ "|>"
              a_ [href_ $ T.pack $ baseUrl ++ "..."] "..."
          )
        div_ [id_ "main"] (div_ [id_ "page"] (nodeHtml ast))
        footer_
          ( a_ [href_ $ T.pack $ baseUrl ++ "index.html"] "✿ thefireflyer ✿"
          )
    )

-------------------------------------------------------------------------------

nodeHtml :: Node -> Html ()
nodeHtml (Node _ nT children) =
  case nT of
    DOCUMENT -> div_ [class_ "page-inner"] (mapM_ nodeHtml children)
    THEMATIC_BREAK -> br_ []
    PARAGRAPH -> p_ (mapM_ nodeHtml children)
    BLOCK_QUOTE -> div_ [class_ "quote"] (mapM_ nodeHtml children)
    HTML_BLOCK x -> toHtmlRaw (T.unpack x)
    HEADING 1 -> h1_ (mapM_ nodeHtml children)
    HEADING 2 -> h2_ (mapM_ nodeHtml children)
    HEADING 3 -> h3_ (mapM_ nodeHtml children)
    LIST (ListAttributes ORDERED_LIST _ _ _) -> ol_ (mapM_ nodeHtml children)
    LIST _ -> ul_ (mapM_ nodeHtml children)
    ITEM -> li_ (mapM_ nodeHtml children)
    HTML_INLINE x -> toHtmlRaw (T.unpack x)
    EMPH -> i_ (mapM_ nodeHtml children)
    STRONG -> b_ (mapM_ nodeHtml children)
    LINK url _ -> a_ [href_ url] (mapM_ nodeHtml children)
    CODE_BLOCK info x ->
      div_
        [class_ "code-holder"]
        ( div_
            [class_ "code"]
            ( do
                p_ (toHtml (T.unpack info))
                pre_ (toHtml (T.unpack x))
            )
        )
    TEXT x -> toHtml (T.unpack x)
    _ -> mapM_ nodeHtml children

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

embedInTemplate :: String -> IO String
embedInTemplate inner = do
  template <- lines <$> readFile (inputPath ++ "/template/template.html")
  let (left, middle) = span (/= "%%body%%") template
  let (_, right) = span (== "%%body%%") middle
  let whole = unlines left ++ inner ++ unlines right
  pure whole

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
