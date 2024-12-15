module Trials.FileTrials where

import Data.Maybe qualified
import System.Random (Random (randomRs), RandomGen)
import Text.Read (readMaybe)

-- ////////////////////////////////////////////////

parseIntDefaultZero :: String -> Int
parseIntDefaultZero item =
  let num = readMaybe item :: Maybe Int
   in Data.Maybe.fromMaybe 0 num

sumsInLine :: String -> Int
sumsInLine input =
  let inputLines = words input
   in foldr ((+) . parseIntDefaultZero) 0 inputLines

readWriteSums :: String -> String
readWriteSums input =
  let inputLines = splitLines input
      values = map sumsInLine inputLines
   in -- total = sum values
      --  in unlines ("-----LINES-----" : map show values ++ "-----TOTAL-----" : [show total])
      unlines ("-----LINES-----" : map show values)

-- ////////////////////////////////////////////////

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
   in pre : case suf of
        ('\r' : '\n' : rest) -> splitLines rest
        ('\r' : rest) -> splitLines rest
        ('\n' : rest) -> splitLines rest
        _ -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

-- ////////////////////////////////////////////////

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

-- ////////////////////////////////////////////////

writeRandomValues :: (RandomGen p1) => Int -> p1 -> p2 -> String
writeRandomValues num rng _ =
  let infiniteRandoms = randomRs (0, 1000000000) rng
   in unlines (map (f infiniteRandoms) [1 .. num])
  where
    f infiniteRandoms i = foldr (formatNumbers . show) "" (generateNumbers i infiniteRandoms)

generateNumbers :: Int -> [Integer] -> [Integer]
generateNumbers i infiniteRandoms =
  let (_, xs) = splitAt ((i - 1) * numberOfGeneratedNumbers) (take (i * numberOfGeneratedNumbers) infiniteRandoms :: [Integer])
   in xs
  where
    numberOfGeneratedNumbers = 5

formatNumbers :: [Char] -> [Char] -> [Char]
formatNumbers acc item = " " ++ acc ++ item

-- ////////////////////////////////////////////////

main :: (RandomGen p1) => [String] -> p1 -> IO ()
main [input, output, "rw"] _ = interactWith readWriteSums input output
main [input, output, "gw", numberOfRows] rng = interactWith (writeRandomValues (parseIntDefaultZero numberOfRows) rng) output input
main _ _ = putStrLn "invalid args"