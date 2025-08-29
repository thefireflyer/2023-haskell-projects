{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module SLang.Pretty
  ( testPretty,
    ansiC,
    ansiCl,
    ansiB,
    ansiBl,
    ansiI,
    ansiIl,
    ansiRs,
    between,
    paren,
    curly,
    squar
  )
where

import Common
import Data.Text.ANSI (ANSI)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import System.Console.Terminal.Size
import Test.HUnit (Test, runTestTT, test, (~:), (~=?), (~?))

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

ansiRs :: String
ansiRs = "\ESC[0m"

ansiC :: Int -> String
ansiC x = "\ESC[38;5;" <> show x <> "m"

ansiCl :: Int -> String -> String
ansiCl x s = ansiC x <> s <> ansiRs

ansiB :: Int -> String
ansiB x = "\ESC[48;5;" <> show x <> "m"

ansiBl :: Int -> String -> String
ansiBl x s = ansiB x <> s <> ansiRs

ansiI :: String
ansiI = "\ESC[3m"

ansiIl :: String -> String
ansiIl s = ansiI <> s <> ansiRs

pCyc :: [Int]
pCyc = [3, 5, 69]

between :: String -> String -> Int -> String -> String
between b0 b1 d s = ansiCl dc b0 <> s <> ansiCl dc b1
  where
    dc = pCyc !! mod d 3

paren :: Int -> String -> String
paren = between "(" ")"

curly :: Int -> String -> String
curly = between "{" "}"

squar :: Int -> String -> String
squar = between "[" "]"

-- parenpre :: Int -> Int -> Int -> String -> String
-- parenpre d p pR s = if p > pR then paren d s else s

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

type PlainText = String

prPlainText :: Stt -> Int -> PlainText
prPlainText = error todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

prANSIText :: Stt -> Int -> ANSI
prANSIText = error todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | Structural text.
data Stt
  = SttId
  | SttH
  | SttV
  | SttText String
  | SttMany (NonEmpty Stt)
  | SttSepBy Stt (NonEmpty Stt)
  | SttBetween Stt Stt Stt
  | SttPrefer Stt
  | SttAvoid Stt
  | SttMaxWidth Stt Int
  deriving (Show)

lengthStt :: Stt -> Int
lengthStt = error todo

instance Semigroup Stt where
  (<>) :: Stt -> Stt -> Stt
  (<>) = error todo

instance Monoid Stt where
  mempty :: Stt
  mempty = SttId

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

testPretty :: IO ()
testPretty = do
  runTestTT tests $> ()

tests :: Test
tests =
  test
    [ "test1" ~: True ~? "foo",
      "test2" ~: True ~=? True
    ]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
