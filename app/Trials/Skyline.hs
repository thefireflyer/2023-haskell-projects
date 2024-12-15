-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Trials.Skyline where

-------------------------------------------------------------------------------

import Control.Monad (foldM_, forM_, when)
import Data.Array.IO
import Data.List (sortOn)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

type Input = [Building]

type Output = [Segment]

-------------------------------------------------------------------------------

data Building = Building
  { b0 :: Int,
    b1 :: Int,
    bH :: Int
  }
  deriving (Show)

-------------------------------------------------------------------------------

data Segment = Segment
  { sL :: Int,
    sH :: Int
  }
  deriving (Show)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

mX :: Int
mX = 80

mH :: Int
mH = 10

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
{------------------------------------------------------------------------------
\|
\|      +-----+
\|      |     |     +---+
\|      |     |     |   |
\|   +----+   |     +---+   +---------+
\|   |  | | +----+  |   |   |         |
\|   |  | | | |  |  |   |   | +----+  |
\|   |  | | | |  |  |   |   | |    |  |
\|   +----+-+----+  +---+   +---------+
\|
\| <-----------------------------------------------------------------------> -}
{------------------------------------------------------------------------------
\|
\|      +-----+
\|      |     |     +---+
\|      |     |     |   |
\|   +--+     |     |   |   +---------+
\|   |        +--+  |   |   |         |
\|   |           |  |   |   |         |
\|   |           |  |   |   |         |
\|---+           +--+   +---+         +----------------------------------------
\|
\| <-----------------------------------------------------------------------> -}
-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

skyline :: Input -> Output
skyline xs =
  inner (pb xs) 0 0 0 []
  where
    pb = sortOn b0
    -- finish the skyline
    inner [] x0 x1 xH ys = Segment (mX - x1) 0 : Segment (x1 - x0) xH : ys
    -- overflows and taller
    inner (b : bs) x0 x1 xH ys
      | b0 b < x1 && b1 b > x1 && bH b > xH =
          inner bs (b0 b) (b1 b) (bH b) (Segment (b0 b - x0) xH : ys)
    -- overflows, but not taller
    inner (b : bs) x0 x1 xH ys
      | b0 b < x1 && b1 b > x1 =
          inner (pb $ Building x1 (b1 b) (bH b) : bs) x0 x1 xH ys
    -- inner and taller
    inner (b : bs) x0 x1 xH ys
      | b0 b < x1 && bH b > xH =
          inner (pb $ Building (b1 b) x1 xH : bs) (b0 b) (b1 b) (bH b) (Segment (b0 b - x0) xH : ys)
    -- inner, but not taller
    inner (b : bs) x0 x1 xH ys
      | b0 b < x1 =
          inner bs x0 x1 xH ys
    -- touching, but not overlapping
    inner (b : bs) x0 x1 xH ys
      | b0 b == x1 =
          inner bs (b0 b) (b1 b) (bH b) (Segment (x1 - x0) xH : ys)
    -- gap between buildings
    inner (b : bs) x0 x1 xH ys =
      inner bs (b0 b) (b1 b) (bH b) (Segment (b0 b - x1) 0 : Segment (x1 - x0) xH : ys)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

renderBuildings :: [Building] -> IO ()
renderBuildings bs =
  do
    let bs' = sortOn b0 bs

    -- let mH = foldr (max . bH) 0 bs
    -- let mX = max (foldr (max . b1) 0 bs) 80

    mat <- newArray ((0, 0), (mX, mH)) ' ' :: IO (IOUArray (Int, Int) Char)

    forM_
      bs'
      ( \b -> do
          writeArray mat (b0 b, mH) '+'
          writeArray mat (b1 b, mH) '+'
          writeArray mat (b0 b, mH - bH b) '+'
          writeArray mat (b1 b, mH - bH b) '+'
          forM_
            [1 .. bH b - 1]
            ( \h -> do
                let p0 = (b0 b, mH - h)
                let p1 = (b1 b, mH - h)

                v0 <- readArray mat p0
                v1 <- readArray mat p1

                when (v0 == ' ') $ writeArray mat p0 '|'
                when (v1 == ' ') $ writeArray mat p1 '|'
            )
          forM_
            [b0 b + 1 .. b1 b - 1]
            ( \x -> do
                let p0 = (x, mH - bH b)
                let p1 = (x, mH)

                v0 <- readArray mat p0
                v1 <- readArray mat p1

                when (v0 /= '+') $ writeArray mat p0 '-'
                when (v1 /= '+') $ writeArray mat p1 '-'
            )
      )

    putChar '\n'
    forM_ [0 .. mX] (\_ -> putChar '_')
    putChar '\n'
    putChar '\n'

    forM_
      [0 .. mH]
      ( \y ->
          do
            forM_
              [0 .. mX]
              ( \x -> putChar =<< readArray mat (x, y)
              )
            putChar '\n'
      )

    forM_ [0 .. mX] (\_ -> putChar '_')
    putChar '\n'

-------------------------------------------------------------------------------

renderSegments :: [Segment] -> IO ()
renderSegments ys = do
  mat <- newArray ((0, 0), (mX, mH)) ' ' :: IO (IOUArray (Int, Int) Char)

  foldM_
    ( \(x, y) s -> do
        let x' = x + sL s
        let y' = sH s

        writeArray mat (x, mH - y') '+'
        writeArray mat (x', mH - y') '+'

        forM_
          [x .. x']
          ( \x'' -> do
              let p = (x'', mH - y')
              v <- readArray mat p
              when (v == ' ') $ writeArray mat p '-'
          )

        forM_
          [(min y y') .. (max y y')]
          ( \y'' -> do
              let p = (x, mH - y'')
              v <- readArray mat p
              when (v /= '+') $ writeArray mat p '|'
          )

        return (x', y')
    )
    (0, 0)
    ys

  putChar '\n'
  forM_
    [0 .. mH]
    ( \y ->
        do
          forM_
            [0 .. mX]
            ( \x -> putChar =<< readArray mat (x, y)
            )
          putChar '\n'
    )

  forM_ [0 .. mX] (\_ -> putChar '_')
  putChar '\n'

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

demo :: [Building]
demo =
  [ Building 4 9 4,
    Building 7 13 7,
    Building 11 16 3,
    Building 19 23 4,
    Building 19 23 6,
    Building 27 37 4,
    Building 29 34 2
  ]

{-
     +-----+
     |     |     +---+
     |     |     |   |
  +----+   |     +---+   +---------+
  |  | | +----+  |   |   |         |
  |  | | | |  |  |   |   | +----+  |
  |  | | | |  |  |   |   | |    |  |
  +----+-+----+  +---+   +---------+
-}

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: IO ()
main = do
  renderBuildings demo
  renderSegments . reverse $ skyline demo

-- print . reverse $ skyline demo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
