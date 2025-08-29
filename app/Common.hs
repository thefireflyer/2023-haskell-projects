{-# LANGUAGE UndecidableInstances #-}

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Common where

-------------------------------------------------------------------------------

import Control.Arrow ((&&&), (|||))
import Flow
import Test.QuickCheck
import Test.HUnit
import Control.Monad (unless)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

todo :: String
todo = "\ESC[31;1mTODO:\ESC[0m function not implemented!"

-------------------------------------------------------------------------------

putError :: String -> IO ()
putError = putStrLn . (++) "\ESC[31;1merror:\ESC[0m "

putWarning :: String -> IO ()
putWarning = putStrLn . (++) "\ESC[33;1mwarning:\ESC[0m "

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Fallible e a = Okay a | Failed [e] deriving (Functor, Show, Eq)

instance Applicative (Fallible e) where
  pure :: a -> Fallible e a
  pure = Okay
  (<*>) :: Fallible e (a -> b) -> Fallible e a -> Fallible e b
  Okay f <*> Okay a = Okay (f a)
  Okay _ <*> Failed ys = Failed ys
  Failed xs <*> Okay _ = Failed xs
  Failed xs <*> Failed ys = Failed (xs<>ys)

instance Monoid a => Monoid (Fallible e a) where 
  mempty :: Fallible e a
  mempty = Okay mempty

instance Semigroup (Fallible e a) where
  (<>) :: Fallible e a -> Fallible e a -> Fallible e a
  Okay _ <> Okay y = Okay y
  Okay _ <> Failed ys = Failed ys
  Failed xs <> Okay _ = Failed xs
  Failed xs <> Failed ys = Failed (xs<>ys)

instance Monad (Fallible e) where
  (>>=) :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
  Okay a >>= f = f a
  Failed a >>= _ = Failed a

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

ptest :: (Test.QuickCheck.Testable prop) => prop -> IO ()
ptest p = do
  r <- quickCheckWithResult stdArgs {chatty = False} p
  unless (isSuccess r) $
    putStr "\n\n" *> recheck r p *> assertFailure ""

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- Patrick Thomson's discussion of recursion schemes.
-- https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

newtype Term f = In {out :: f (Term f)}

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type RAlgebra f a = f (Term f, a) -> a

type RCoalgebra f a = a -> f (Either (Term f) a)

deriving instance (Eq (f (Term f))) => Eq (Term f)

-------------------------------------------------------------------------------

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out .> fmap (cata f) .> f

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = In <. fmap (ana f) <. f

para :: (Functor f) => RAlgebra f a -> Term f -> a
para f = out .> fmap (id &&& para f) .> f

apo :: (Functor f) => RCoalgebra f a -> a -> Term f
apo f = In <. fmap (id ||| apo f) <. f

-------------------------------------------------------------------------------

topDown, bottomUp :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
topDown f = ana (out <. f)
bottomUp f = cata (In .> f)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------