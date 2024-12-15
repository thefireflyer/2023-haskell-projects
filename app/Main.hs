-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import Calc.Algebraic.Main (main)
import Calc.Numeric.Main (main)
import DMS.Main (main)
import SLang.Main (main)
import System.Environment (getArgs)
import System.Random (mkStdGen, randomIO)
import Trials.FileTrials (main)
import Trials.Skyline (main)
import Web.Main (main)

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
          _ -> putStrLn "invalid args"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- type CustomerID = Int
-- type Address = String

-- data Customer = Customer {
--       customerID      :: CustomerID
--     , customerName    :: String
--     , customerAddress :: Address
--     } deriving (Show)

-- test (_: xs) = if length xs > 1 then test xs else xs
-- test [] = []

-- main :: IO ()
-- main = print (test ["at", "ab", "ac"])
-- main = print (Customer 0 "test" "testing")
