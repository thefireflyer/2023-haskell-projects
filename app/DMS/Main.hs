{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module DMS.Main where

-------------------------------------------------------------------------------

import Common
import DMS.Config (configPath)
import DMS.Logging (logPath)
import DMS.StaticFolders (normalizeDownloads)
import Data.Text (pack)
import Turtle (inproc, stdout)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- :| init    | Initialize DMS
-- :| nuke    | Completely remove DMS
-- :| recover | Attempt to recover a corrupted DMS

-- :| config | Open DMS config
-- :| logs   | Open DMS logs

-- :| https     | Configure HTTPS API
-- :| tcp       | Configure TCP API
-- :| bluetooth | Configure bluetooth API

-- :| q | Process query
-- :| a | Process action
-- :| i | Start REPL

-------------------------------------------------------------------------------

-- Query {
--
--
--
-- }

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
-------------------------------------------------------------------------------

main ["init"] = todo
main ["nuke"] = todo
main ["recover"] = todo

-------------------------------------------------------------------------------

main ["config"] = stdout (inproc "xdg-open" [pack configPath] "")
main ["logs"] = stdout (inproc "xdg-open" [pack logPath] "")

-------------------------------------------------------------------------------

main ["https"] = todo
main ["tcp"] = todo
main ["bluetooth"] = todo

-------------------------------------------------------------------------------

main ("q" : _) = todo
main ("a" : _) = todo
main ["i"] = todo

-------------------------------------------------------------------------------

main ["rr"] = DMS.StaticFolders.normalizeDownloads

-------------------------------------------------------------------------------

main (x:_) = putError ("unknown command " ++ show x) *> help
main _ = help

-------------------------------------------------------------------------------

help :: IO ()
help = mapM_ putStrLn 
            ["Usage: test0 dms <command>"
            ,"Commands:"
            -- ,"    init    Initialize DMS"
            -- ,"    nuke    Completely remove DMS"
            ,"    config  Open DMS config"
            ,"    logs    Open DMS logs"
            ,"    rr      Run refresh"]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
