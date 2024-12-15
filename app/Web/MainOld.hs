-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Web.MainOld where

-------------------------------------------------------------------------------

import Common (todo)
import Control.Concurrent (threadDelay)
import Control.Foldl qualified
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text (pack, splitOn, unpack)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Parsec (endBy, eof, letter, many, noneOf, parse, sepBy, try, (<?>), (<|>))
import Text.Parsec.Char (char, string)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (many1)
import Text.Read (readMaybe)
import Turtle (datefile, dropExtension, extension, fold, lstree, rmtree)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

srcDirPath :: FilePath
srcDirPath = "/home/casey/dev/hsk/test0/site/" -- "/home/casey/0/glob/out/thefireflyer/"

siteCSSPath :: FilePath
siteCSSPath = srcDirPath ++ "main.css"

siteJSPath :: FilePath
siteJSPath = srcDirPath ++ "main.js"

siteOutPath :: FilePath
siteOutPath = "/home/casey/dev/hsk/test0/appdata/site/"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
{-

\| build | Build static site from src
\| clear | Remove all generated files
\| watch | Watch src dir and re-build pages that change

\| config | Open config
\| logs   | Open logs

-}
-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main ["build"] = buildSiteMap >>= buildDynamic >>= buildStatic >>= serve
main ["watch"] = todo -- we need a way to have one thread check for updates, and another serve the static site.
main ["clear"] = rmtree siteOutPath
main _ = error "invalid args"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

buildSiteMap :: IO ()
buildSiteMap = todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

buildDynamic = todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

buildStatic = todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

serve = todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-- -------------------------------------------------------------------------------

-- main [] =
--   do
--     putStrLn "Loading resources..."
--     siteCSS <- readFile siteCSSPath
--     siteJS <- readFile siteJSPath
--     putStrLn "Loading cache"
--     cacheContents <- readFile cacheFilePath
--     let cacheData = fromMaybe (posixSecondsToUTCTime 0) (readMaybe cacheContents)
--     putStrLn "Refreshing..."
--     cacheData' <- innerLoop siteCSS siteJS cacheData
--     _ <- writeFile cacheFilePath (show cacheData')
--     putStrLn "Exiting..."
--   where
--     cacheFilePath = siteOutPath ++ "cache"
--     innerLoop :: String -> String -> UTCTime -> IO UTCTime
--     innerLoop siteCSS siteJS cacheData =
--       do
--         -- putStrLn (foldMap (const "-") [0 .. 80])
--         files <-
--           Turtle.fold
--             ( do
--                 path <- lstree srcDirPath
--                 time <- datefile path
--                 return (path, time, extension path)
--             )
--             Control.Foldl.list
--         let files' = filter (\(_, t, e) -> t > cacheData && e == Just "md") files
--         unless (null files') (putStrLn "Updating...")
--         mapM_ (buildPage siteCSS siteJS) files'
--         unless (null files') (putStrLn "Up to date!")
--         currentTime <- getCurrentTime
--         threadDelay 1000
--         return (posixSecondsToUTCTime 0)

--     buildPage siteCSS siteJS (path, _, _) =
--       do
--         let path' = drop 6 (splitOn (pack "/") (pack (dropExtension path)))
--         print path'
--         fileContent <- readFile path
--         case parseMarkdown fileContent of
--           Left err -> print err
--           Right markdown -> do
--             let out = page markdown [] siteCSS siteJS
--             writeFile (foldl (\r x -> r ++ "-" ++ unpack x) siteOutPath path' ++ ".html") out
--             return ()

-- main [] =
--   do
--     putStrLn "Updating..."
--     _css <- readFile "/home/casey/dev/hsk/test0/app/Web/main.css"
--     _js <- readFile "/home/casey/dev/hsk/test0/app/Web/main.js"
--     _inner <- readFile "/home/casey/dev/hsk/test0/app/Web/sillet.md"
--     let _inner' = parseSource (unlines (filter (/= "") (lines _inner)))
--     let _inner'' = fromRight (error (show _inner')) _inner'
--     let _path = [("TheFireFlyer", "/"), ("2024", "/2024/"), ("Demo", "")]
--     let raw = page _inner'' _path _css _js
--     -- putStrLn raw
--     writeFile "/tmp/out.html" raw
--     putStrLn "Finished update"
-- main ["watch"] = do
--   now <- getCurrentTime
--   monitor "/home/casey/dev/hsk/test0/app/Web/sillet.md" now (main [])
-- main _ = error "invalid args"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

monitor file mtime handler = do
  threadDelay 1000
  t <- datefile file
  if t > mtime
    then handler >> monitor file t handler
    else monitor file mtime handler

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data MarkdownUnit = MarkdownUnit
  { title :: String,
    top :: [MarkdownBody],
    sections :: [MarkdownUnit]
  }
  deriving (Show)

-------------------------------------------------------------------------------

data MarkdownBody
  = OrderedList
  | UnorderedList
  | Code
  | Paragraph [[InlineMarkdown]]
  | Comment
  deriving (Show)

-------------------------------------------------------------------------------

data InlineMarkdown
  = Bold [InlineMarkdown]
  | Italic [InlineMarkdown]
  | Raw String
  | Embed String String
  | Link [InlineMarkdown] String
  | Html
  | Plain Char
  deriving (Show)

-------------------------------------------------------------------------------

-- parseMarkdown :: String -> Either ParseError ()
parseMarkdown = parse _s "Unknown"
  where
    _s = many _chapter

    _chapter =
      do
        title <-
          ( do
              _ <- string "# "
              x <- many (noneOf "\r\n")
              _ <- _eol
              return x
            )
            <|> ( do
                    _ <- string "<h1"
                    _ <- many (noneOf ">")
                    _ <- string ">"
                    x <- many (noneOf "<")
                    _ <- string "</h1>"
                    _ <- _eol
                    return x
                )
            <?> "chapter title"
        top <- _body
        sections <- many (try _section)
        return (MarkdownUnit title top sections)

    _section =
      do
        _ <- string "## "
        title <- many (noneOf "\r\n")
        _ <- _eol
        top <- _body
        subsections <- many (try _subsection)
        return (MarkdownUnit title top subsections)

    _subsection =
      do
        _ <- string "### "
        title <- many (noneOf "\r\n")
        _ <- _eol
        body <- _body
        return (MarkdownUnit title body [])

    _body =
      sepBy
        ( try _comment
            <|> try _ol
            <|> try _ul
            <|> try _code
            <|> try _paragraph
        )
        _eol
        <?> "body"

    _comment =
      do
        _ <- string "<!--"
        _ <- many (try (noneOf "-"))
        _ <- string "-->"
        return Comment

    _ol = do string "1. "; return OrderedList

    _ul = do string "- "; return UnorderedList

    _code =
      do
        _ <- string "```"
        lang <- many (noneOf "\n\r")
        _ <- _eol
        inner <- many (noneOf "`")
        _ <- string "```"
        return Code

    _paragraph = Paragraph <$> endBy _chunk _eol <?> "paragraph"
    -- do
    -- _ <- endBy (many1 (noneOf "\n\r")) _eol
    -- return ()

    _chunk = many1 (try _bold <|> try _italic <|> try _literal <|> try _embed <|> try _link <|> try _html <|> try (Plain <$> _plain) <?> "chunk")

    _bold =
      do
        _ <- string "**"
        inner <- _chunk
        _ <- string "**"
        return (Bold inner)

    _italic =
      do
        _ <- char '_'
        inner <- _chunk
        _ <- char '_'
        return (Italic inner)

    _literal =
      do
        _ <- char '`'
        inner <- many (noneOf "`\r\n")
        _ <- char '`'
        return (Raw inner)

    _embed =
      do
        _ <- string "!["
        desc <- many (noneOf "]\r\n")
        _ <- string "]("
        href <- many (noneOf ")\r\n")
        _ <- char ')'
        return (Embed desc href)

    _link =
      do
        _ <- char '['
        inner <- _chunk
        _ <- string "]("
        href <- many (noneOf ")\r\n")
        _ <- char ')'
        return (Link inner href)

    _html = do _ <- string "<>"; return Html

    _plain = noneOf "\n\r*_]#"

    _eol =
      try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
        <|> string "\r"
        <?> "end of line"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Source = Source
  { sourceTitle :: String,
    sourceContent :: [RichLine]
  }
  deriving (Show)

data RichLine = RichLine
  { rlElem :: RichElem,
    rlText :: [RichText]
  }
  deriving (Show)

data RichElem
  = RH1
  | RH2
  | RH3
  | ROl
  | RUl
  | RP
  deriving (Show, Eq)

data RichText = RichText
  { rtBold :: Bool,
    rtItalic :: Bool,
    rtHref :: Maybe String,
    rtInner :: String
  }
  deriving (Show)

-------------------------------------------------------------------------------

parseSource :: String -> Either ParseError Source
parseSource = parse _s "Unknown"
  where
    _s = do
      metadata <- _yaml
      _ <- string "---"
      content <- _md
      let title = _fprop "title" metadata
      _ <- eof
      return (Source title content)

    _fprop tar ((left, right) : _) | left == tar = right
    _fprop tar (_ : xs) = _fprop tar xs
    _fprop _ _ = error "Missing prop!"

    _eol =
      try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
        <|> string "\r"
        <?> "end of line"

    _yaml = endBy _yaml_line _eol
    _yaml_line =
      do
        left <- many1 letter
        _ <- string ": "
        right <- many1 (noneOf "\n\r")
        return (left, right)

    _md = endBy _md_line _eol
    _md_line =
      do
        left <- _md_h3 <|> _md_h2 <|> _md_h1 <|> _md_ol <|> _md_ul <|> return RP
        right <- many _md_rt
        return (RichLine left right)

    _md_h3 = try (do _ <- string "### "; return RH3)
    _md_h2 = try (do _ <- string "## "; return RH2)
    _md_h1 = try (do _ <- string "# "; return RH1)
    _md_ol = try (do _ <- string "+ "; return ROl)
    _md_ul = try (do _ <- string "- "; return RUl)
    _md_rt =
      try
        ( do
            _ <- string "***"
            inner <- many1 (noneOf "*")
            _ <- string "***"
            return (RichText True True Nothing inner)
        )
        <|> try
          ( do
              _ <- string "**"
              inner <- many1 (noneOf "*")
              _ <- string "**"
              return (RichText True False Nothing inner)
          )
        <|> try
          ( do
              _ <- char '*'
              inner <- many1 (noneOf "*")
              _ <- char '*'
              return (RichText False True Nothing inner)
          )
        <|> try
          ( do
              _ <- char '_'
              inner <- many1 (noneOf "_")
              _ <- char '_'
              return (RichText False True Nothing inner)
          )
        <|> try
          ( do
              _ <- char '['
              inner <- many1 (noneOf "]")
              _ <- string "]("
              href <- many1 (noneOf ")")
              _ <- string ")"
              return (RichText False False (Just href) inner)
          )
        <|> do RichText False False Nothing <$> _md_t
    _md_t = many1 (noneOf "\n\r*_[")

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

page :: [MarkdownUnit] -> [(String, String)] -> String -> String -> String
page src path _css _js =
  "<!DOCTYPE html>\n"
    ++ writeHTML
      ( hHtml
          [ hHead
              [ hMeta [Keyval "charset" "utf-8"],
                hMeta [Keyval "name" "viewport", Keyval "content" "height=device-height"],
                hMeta [Keyval "name" "description", Keyval "content" "personal website"],
                hLink [Keyval "rel" "icon", Keyval "href" "https://thefireflyer.vercel.app/homeportal.png"],
                hLink [Keyval "rel" "stylesheet", Keyval "href" "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,400,1,0"],
                hLink [Keyval "rel" "preconnect", Keyval "href" "https://fonts.googleapis.com"],
                hLink [Keyval "rel" "preconnect", Keyval "href" "https://fonts.gstatic.com", Key "crossorigin"],
                hLink [Keyval "rel" "stylesheet", Keyval "href" "https://fonts.googleapis.com/css2?family=Cormorant:wght@700&family=Fira+Code&display=swap"],
                hScript [] [Literal _js],
                hScript [Keyval "id" "MathJax-script", Key "async", Keyval "src" "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"] [],
                hTitle reversePath,
                hCss _css
              ],
            hBody
              [ hHeader
                  [ -- Node "button" [] [Node "span" [Keyval "class" "material-symbols-outlined"] [Literal "menu"]],
                    --   Node "button" [] [Node "span" [Keyval "class" "material-symbols-outlined"] [Literal "search"]],
                    --   p [Keyval "style" "flex: 1;"] [],
                    hP [] forwardPath,
                    -- p [Keyval "style" "flex: 1;"] [],
                    hTextInput "Search"
                    -- Node "button" [] [Node "span" [Keyval "class" "material-symbols-outlined"] [Literal "more_horiz"]],
                    -- Node "button" [] [Node "span" [Keyval "class" "material-symbols-outlined"] [Literal "history"]]
                  ],
                hDiv
                  [Keyval "id" "main"]
                  [ -- _div
                    --   [Keyval "id" "toc"]
                    --   [ h3 [] [Literal "Contents"]
                    --   ],
                    hDiv
                      [Keyval "id" "cover"]
                      [hH1 [Keyval "id" "title"] [Literal "..."]],
                    hDiv
                      [Keyval "id" "page"]
                      ( pageContent
                          ++ [ hH1 [] [Literal "References"],
                               hDiv
                                 [Keyval "class" "page-inner"]
                                 [ hP [] [Literal "..."]
                                 ]
                                 --  , _div
                                 --    [Keyval "id" "toc"]
                                 --    [ p [] [Node "b" [] [Literal "Contents"]],
                                 --      Node
                                 --        "ol"
                                 --        []
                                 --        [ Node "li" [] [_a [] [Literal "Introduction"]],
                                 --          Node "li" [] [_a [] [Literal "Methodology"]],
                                 --          Node "li" [] [_a [] [Literal "Conclusion"]],
                                 --          Node "li" [] [_a [] [Literal "References"]]
                                 --        ]
                                 --    ]
                             ]
                      ),
                    --   [ _div
                    --       [Keyval "class" "page-inner"]
                    --       [p [] [Literal "Abstract. Testing testing."]]
                    --   ]
                    hDiv [Keyval "id" "ender"] []
                  ],
                hFooter
                  [ hP [] [hA [Keyval "href" "/"] [Literal "✿ thefireflyer ✿"]],
                    hP
                      []
                      [ hA [Keyval "href" ""] [Literal "GitHub"],
                        Literal " • ",
                        hA [Keyval "href" ""] [Literal "Fedi"]
                      ]
                  ]
              ]
          ]
      )
      0
  where
    forwardPath = foldMap ff path
    reversePath = foldr rf "" path
    ff (x, "") = [Literal x]
    ff (x, xx) = [hA [Keyval "href" xx] [Literal x], Literal ">>="]
    rf (x, "") r = r ++ x
    rf (x, _) r = r ++ " =<< " ++ x

    pageContent = []

-- _fRichElem :: RichElem -> RichLine -> [[RichLine]] -> [[RichLine]]
-- _fRichElem t x (xs : xss) | rlElem x == t = [] : (x : xs) : xss
-- _fRichElem _ x (xs : xss) = (x : xs) : xss
-- _fRichElem _ x [] = [[x]]

-- pageChapters = foldr (_fRichElem RH1) [] (sourceContent src)
-- -- pageChapters' = map (foldr (_fRichElem RH2) []) pageChapters
-- -- pageChapters'' = map (map (foldr (_fRichElem RH3) [])) pageChapters'

-- pageContent = foldMap _fC1 pageChapters
-- _fC1 :: [RichLine] -> [Node]
-- _fC1 (top : chapter)
--   | rlElem top == RH1 =
--       [ hH1 [] [Literal (foldMap rtInner (rlText top))],
--         hDiv [Keyval "class" "page-inner"] (map _fC2 chapter)
--       ]
-- _fC1 (_ : chapter) =
--   [ hDiv [Keyval "class" "page-inner"] (map _fC2 chapter)
--   ]
-- _fC1 _ = error "???"

-- _fC2 :: RichLine -> Node
-- _fC2 x = case rlElem x of
--   RH1 -> error "???"
--   RH2 -> hH2 [] (map _fC3 (rlText x))
--   RH3 -> hH3 [] (map _fC3 (rlText x))
--   ROl -> todo
--   RUl -> todo
--   RP -> hP [] (map _fC3 (rlText x))

-- _fC3 :: RichText -> Node
-- _fC3 x | rtBold x = Node "b" [] [_fC3 (x {rtBold = False})]
-- _fC3 x | rtItalic x = Node "i" [] [_fC3 (x {rtItalic = False})]
-- _fC3 (RichText {rtHref = (Just href), rtInner = inner}) = Node "a" [Keyval "href" href] [Literal inner]
-- _fC3 (RichText {rtInner = inner}) = Literal inner

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

hHtml :: [Node] -> Node
hHtml = Node "html" [Keyval "lang" "en"]

-------------------------------------------------------------------------------

hHead :: [Node] -> Node
hHead = Node "head" []

-------------------------------------------------------------------------------

hScript :: [Prop] -> [Node] -> Node
hScript = Node "script"

-------------------------------------------------------------------------------

hCss :: String -> Node
hCss x = Node "style" [Keyval "type" "text/css"] [Literal x]

-------------------------------------------------------------------------------

hMeta :: [Prop] -> Node
hMeta = Leaf "meta"

-------------------------------------------------------------------------------

hLink :: [Prop] -> Node
hLink = Leaf "link"

-------------------------------------------------------------------------------

hTitle :: String -> Node
hTitle x = Node "title" [] [Literal x]

-------------------------------------------------------------------------------

hBody :: [Node] -> Node
hBody = Node "body" []

-------------------------------------------------------------------------------

hHeader :: [Node] -> Node
hHeader = Node "header" []

-------------------------------------------------------------------------------

hFooter :: [Node] -> Node
hFooter = Node "footer" []

-------------------------------------------------------------------------------

hDiv :: [Prop] -> [Node] -> Node
hDiv = Node "div"

-------------------------------------------------------------------------------

hH1 :: [Prop] -> [Node] -> Node
hH1 = Node "h1"

-------------------------------------------------------------------------------

hH2 :: [Prop] -> [Node] -> Node
hH2 = Node "h2"

-------------------------------------------------------------------------------

hH3 :: [Prop] -> [Node] -> Node
hH3 = Node "h3"

-------------------------------------------------------------------------------

hP :: [Prop] -> [Node] -> Node
hP = Node "p"

-------------------------------------------------------------------------------

hA :: [Prop] -> [Node] -> Node
hA = Node "a"

-------------------------------------------------------------------------------

hBr :: Node
hBr = Leaf "br" []

-------------------------------------------------------------------------------

hTextInput :: String -> Node
hTextInput placeholder =
  Leaf
    "input"
    [ Keyval "type" "text",
      Keyval "placeholder" placeholder
    ]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Node
  = Leaf String [Prop]
  | Node String [Prop] [Node]
  | Literal String

-------------------------------------------------------------------------------

data Prop
  = Keyval String String
  | Key String

-------------------------------------------------------------------------------

writeHTML :: Node -> Int -> String
writeHTML (Leaf tag props) depth = tabs depth ++ "<" ++ tag ++ writeProps props ++ ">\n"
writeHTML (Node tag props children) depth = tabs depth ++ "<" ++ tag ++ writeProps props ++ ">\n" ++ foldr (\x y -> writeHTML x (depth + 1) ++ y) "" children ++ tabs depth ++ "</" ++ tag ++ ">\n"
writeHTML (Literal x) depth = unlines (map (\x' -> tabs depth ++ x') (lines x))

-------------------------------------------------------------------------------

writeProps :: [Prop] -> String
writeProps = foldr f ""
  where
    f (Keyval key val) r = " " ++ key ++ "=\"" ++ val ++ "\"" ++ r
    f (Key key) r = key ++ " " ++ r

-------------------------------------------------------------------------------

tabs :: Int -> String
tabs depth = foldr (\_ y -> y ++ "    ") "" [1 .. depth]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
