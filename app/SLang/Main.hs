module SLang.Main (main) where

import System.Console.Terminal.Size
import Common
import SLang.Pretty
import SLang.DeBruijn
import SLang.Typechecker
import SLang.Interpreter
import Data.Char (isAlpha, toUpper)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main ["run"] = error todo
main ["check"] = error todo
main ["lint"] = error todo
main ["repl"] = error todo
main ["test"] = runTests internalTests
main ["test", "list"] = mapM_ (putStrLn . fst) internalTests
main ("test":mask) = runTests $ filter (flip elem mask . fst) internalTests
main _ = mapM_ putStrLn
  ["Usage: test0 sl <command>"
  ,"Commands:"
  ,"    run   [file]    Interpreter"
  ,"    check [file]    Typechecker"
  ,"    lint  [file]    Linter"
  ,"    repl            Interactive repl"
  ,"    test  [mask]    Internal tests"
  ,""
  ,"If no mask is provided, test will run all tests."
  ,"Run \ESC[38;5;247mtest0 sl test list\ESC[0m for available tests."]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

internalTests :: [(String, IO ())]
internalTests =
  [("pretty", testPretty)
  ,("de-bruijn", testDeBruijn)
  ,("interpreter", testInterpreter)
  ,("typechecker", testTypechecker)]

runTests :: [(String, IO ())] -> IO ()
runTests ts = do 
  w <- maybe 80 width <$> size
  mapM_ (rT w) ts
  where 
    rT w (s, f) = do
      putStr $ 
        "\ESC[38;5;168m" <> bar 2 <> "\ESC[0m "
        <> fm s
        <> " \ESC[38;5;168m" <> bar (w-5-length s) <> "\ESC[0m\n\n"
      _ <- f
      putChar '\n'

bar :: Int -> String  
bar w = map (const '━') [0..w-1]

fm :: String -> String
fm [] = ""
fm (x:xs) = if isAlpha x then toUpper x:fm' xs else x:fm' xs
  where
  fm' (' ':x:xs) | isAlpha x = ' ':toUpper x:fm' xs
  fm' (p:'-':q:xs) | isAlpha p && isAlpha q = p:' ':toUpper q:fm' xs
  fm' (x:xs) = x:fm' xs
  fm' [] = []

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
