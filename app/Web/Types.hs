module Web.Types where

import CMarkGFM qualified as MD
import Common
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Reflection (Given, give)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Flow
import Lucid (Html)
import System.OsPath (OsPath)
import System.OsPath qualified as OP
import Text.Parsec qualified as P

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Uri
  = Url Scheme Domain Port Path Parameters Anchor
  | Local Path

-- | (i.e., "http", "https", "ws", "wss")
type Scheme = ShortText

type Domain = ShortText

type Port = Int

newtype Path = Path [ShortText] deriving (Semigroup)

newtype Parameters = Para [(ShortText, ShortText)] deriving (Semigroup)

type Anchor = ShortText

-------------------------------------------------------------------------------

uriParser :: P.Parsec Text () String
uriParser =
  (scheme *> P.string "://" *> P.many P.anyChar)
    P.<|> P.many P.anyChar
  where
    scheme =
      P.string "http"
        P.<|> P.string "https"
        P.<|> P.string "ws"
        P.<|> P.string "wss"
        P.<|> P.string "file"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Cfg = Cfg
  { -- | Scheme
    scheme :: !Scheme,
    -- | Domain name
    domain :: !Domain,
    -- | Port
    port :: !(Maybe Port),
    -- | Url prefix
    prefix :: !Path,
    -- | Path to source directory
    srcPath :: !OsPath,
    -- | [Export] Path to export build files to
    outPath :: !OsPath,
    -- | [Dev] websocket address
    wsAddr :: !String,
    -- | [Dev] websocket port
    wsPort :: !Port
  }

defaultCfg :: IO Cfg
defaultCfg = do
  let srcPath_ = "/home/casey/dev/2023-haskell-projects/site/"
      outPath_ = "/home/casey/dev/hsk/test0/appdata/site/"

  srcPath <- OP.encodeFS srcPath_
  outPath <- OP.encodeFS outPath_
  let prefix = TS.pack outPath_ |> TS.split (== '/') |> Path
  pure
    Cfg
      { scheme = TS.pack "file",
        domain = TS.pack "",
        port = Nothing,
        prefix,
        srcPath,
        outPath,
        wsAddr = "localhost",
        wsPort = 8083
      }

-- | Run a function with the default config.
giveCfg :: ((Given Cfg) => IO a) -> IO a
giveCfg f = defaultCfg >>= (`give` f)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

type Parse = (Given Cfg) => ByteString -> MD.Node

type Render = (Given Cfg) => MD.Node -> Html ()

type Trans a = (Given Cfg) => a -> a

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | Node diff.
data DNode
  = -- | Modified node.
    DMod
      { dChilds :: IntMap DNode,
        dText :: Maybe Text,
        dAttr :: Map Text Text
      }
  | -- | Append/Replace node.
    DNew (Html ())
  | -- | Delete node.
    DDel
  | D0
  deriving (Show)

-- diff :: Html () -> Html () -> DNode
-- diff a b = if a == b then D0 else D0

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | Action message sent to dev ws client.
data Action
  = Mutate Text Text
  | Refresh
  | Error Text
  deriving (Show)

serializeAction :: Action -> IO ByteString
serializeAction a = pure $ case a of
  Mutate path ds ->
    B.pack [0]
      <> T.encodeUtf8 path
      <> B.pack [0]
      <> T.encodeUtf8 ds
  Refresh -> B.pack [1]
  Error msg -> B.pack [2] <> T.encodeUtf8 msg

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
