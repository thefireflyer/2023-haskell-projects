{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Common where

-------------------------------------------------------------------------------

import Control.Arrow ((&&&), (|||))
import Flow

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

todo :: a
todo = error "\ESC[31;1mTODO:\ESC[0m function not implemented!"

-------------------------------------------------------------------------------

putError :: String -> IO ()
putError = putStrLn . (++) "\ESC[31;1merror:\ESC[0m "

putWarning :: String -> IO ()
putWarning = putStrLn . (++) "\ESC[33;1mwarning:\ESC[0m "

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