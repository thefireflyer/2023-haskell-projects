-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import Calc.Algebraic.Main (main)
import Calc.Numeric.Main (main)
import DMS.Main (main)
import SLang.Main (main)
import System.Environment (getArgs, getProgName)
import System.Random (mkStdGen, randomIO)
import Trials.FileTrials (main)
import Trials.Skyline (main)
import Web.Main (main)
import Common (putError)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: IO ()
main =
  do
    args <- getArgs
    seed <- randomIO :: IO Int
    let rng = mkStdGen seed
     in case args of
          ("dms" : xs) -> DMS.Main.main xs
          ("web" : xs) -> Web.Main.main xs
          ["num"] -> Calc.Numeric.Main.main
          ["cas"] -> Calc.Algebraic.Main.main
          ("sl" : xs) -> SLang.Main.main xs
          ("ftrials" : xs) -> Trials.FileTrials.main xs rng
          ["skyline"] -> Trials.Skyline.main
          (x:_) -> 
            putError ("unknown subsystem " ++ show x)
            *> help
          _ -> help

-------------------------------------------------------------------------------

help :: IO ()
help = do
  name <- getProgName
  mapM_ putStrLn
            ["Usage: " ++ name ++ " <subsystem>"
            ,"Subsystems:"
            ,"    dms [options]    Document Management System"
            ,"    web [options]    Website tooling"
            ,"    num              Numeric calculator"
            ,"    cas              Computer Algebra System"
            ,"    sl  [options]    Simple Language"
            ,"    ftrials"
            ,"    skyline"]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------