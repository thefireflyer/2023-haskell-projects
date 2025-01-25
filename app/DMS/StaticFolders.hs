{-# OPTIONS_GHC -Wno-missing-signatures #-}

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module DMS.StaticFolders where

-------------------------------------------------------------------------------

-- import DMS.Logging (lcc)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Turtle
  ( MonadIO (liftIO),
    Shell,
    UTCTime,
    datefile,
    extension,
    filename,
    ls,
    mv,
    sh,
  )

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

path = "/home/casey/Downloads/"

imagePrefix = "/home/casey/Downloads/by-date: images/"

videoPrefix = "/home/casey/Downloads/by-date: videos/"

execPrefix = "/home/casey/Downloads/by-type: exec/"

docsPrefix = "/home/casey/Downloads/by-type: docs/"

compressedPrefix = "/home/casey/Downloads/by-type: compressed/"

codePrefix = "/home/casey/Downloads/by-type: code/"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- Renames images & videos to their date-created value
-- Place images in images folder
-- Place videos in videos folder

normalizeDownloads :: IO ()
normalizeDownloads =
  sh
    ( ls path >>= contextualize >>= normalizeFile
    )

-------------------------------------------------------------------------------

contextualize :: FilePath -> Shell (FilePath, UTCTime, String, String)
contextualize path =
  do
    date <- datefile path
    return (path, date, name, ext)
  where
    name = filename path
    ext = map toLower (fromMaybe "" (extension path))

-------------------------------------------------------------------------------

normalizeFile :: (FilePath, UTCTime, String, String) -> Shell ()
normalizeFile (path, date, name, ext)
  | managedFormat ext =
      liftIO
        ( do
            putStrLn ("path=" ++ path)
            putStrLn ("date=" ++ show date)
            putStrLn ("new path=" ++ newPath)
            putStrLn ""
            mv path newPath
        )
  where
    newPath = normalizePath date name ext
normalizeFile _ = return ()

-------------------------------------------------------------------------------

normalizePath :: UTCTime -> String -> String -> String
normalizePath date _ ext | image ext = imagePrefix ++ shorten (show date) ++ "." ++ ext
normalizePath date _ ext | video ext = videoPrefix ++ shorten (show date) ++ "." ++ ext
normalizePath _ name ext | exec ext = execPrefix ++ name
normalizePath _ name ext | docs ext = docsPrefix ++ name
normalizePath _ name ext | compressed ext = compressedPrefix ++ name
normalizePath _ name ext | code ext = codePrefix ++ name
normalizePath _ _ _ = error "wtf"

-------------------------------------------------------------------------------

shorten :: String -> String
-- shorten date = fst (break (== '.') date)
shorten = id

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

managedFormat :: String -> Bool
managedFormat x =
  image x || video x || exec x || docs x || compressed x || code x

-------------------------------------------------------------------------------

image :: String -> Bool
image "png" = True
image "jpg" = True
image "jpeg" = True
image "webp" = True
image "gif" = True
image "svg" = True
image "exr" = True
image "hdr" = True
image _ = False

-------------------------------------------------------------------------------

video :: String -> Bool
video "mov" = True
video "mp4" = True
video "mkv" = True
video "webm" = True
video _ = False

-------------------------------------------------------------------------------

exec :: String -> Bool
exec "exe" = True
exec _ = False

-------------------------------------------------------------------------------

docs :: String -> Bool
docs "pdf" = True
docs "doc" = True
docs "docx" = True
docs "xlsx" = True
docs "txt" = True
docs "odt" = True
docs _ = False

-------------------------------------------------------------------------------

compressed :: String -> Bool
compressed "zip" = True
compressed "tgz" = True
compressed "gz" = True
compressed "vsix" = True
compressed "iso" = True
compressed "qcow2" = True
compressed _ = False

-------------------------------------------------------------------------------

code :: String -> Bool
code "tex" = True
code "ps" = True
code "json" = True
code "ts" = True
code "py" = True
code "rkt" = True
code _ = False

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
