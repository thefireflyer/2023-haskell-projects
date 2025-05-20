{-# LANGUAGE BlockArguments #-}

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module SLang.Main where

-------------------------------------------------------------------------------
import Common
import Control.Monad (join)
import Data.HashMap.Lazy qualified as HML
import Flow

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | S-expression initial algebra.
-- * w is the annotation functor.
-- * a is the terminal type.
-- * b is the recursive type.
data SF w a b
  = SA a
  | SF (w b) [w b]
  deriving (Show, Functor, Eq)

-- | S-expression fixpoint.
type S w a = Term (SF w a)

-- | S-expression annotation functor.
data SW e a = SOkay a | SError [e]
  deriving (Show, Functor, Eq)

-- | S-expression terminal type.
data SA
  = SInt Int
  | SStr String
  | SSym String

-- | A simple S-expression language.
type Simple = S (SW String) SA

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

sf :: Simple -> [Simple] -> Simple
sf x xs = In $ SF (pure x) (map pure xs)

sint :: Int -> Simple
sint = In . SA . SInt

sstr :: String -> Simple
sstr = In . SA . SStr

ssym :: String -> Simple
ssym = In . SA . SSym

serr :: String -> Simple
serr x = In $ SF (SError [x]) []

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

instance Applicative (SW a) where
  pure :: a2 -> SW a1 a2
  pure = SOkay
  (<*>) :: SW a1 (a2 -> b) -> SW a1 a2 -> SW a1 b
  SOkay x <*> SOkay y = SOkay (x y)
  SError x <*> SError y = SError (x ++ y)
  SError x <*> _ = SError x
  _ <*> SError y = SError y

instance (Semigroup e, Semigroup a) => Semigroup (SW e a) where
  (<>) :: SW e a -> SW e a -> SW e a
  SOkay x <> SOkay y = SOkay $ x <> y
  SError x <> SError y = SError (x <> y)
  SError x <> _ = SError x
  _ <> SError y = SError y

instance Monad (SW e) where
  (>>=) :: SW e a -> (a -> SW e b) -> SW e b
  SOkay a >>= f = f a
  SError e >>= _ = SError e

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- TODO: figure out nice identing.
prettySimple :: Simple -> String
prettySimple = prettySW . cata prettySF

prettySF :: Algebra (SF (SW String) SA) (SW String String)
prettySF (SA x) = pure $ prettySA x
prettySF (SF x xs) = SOkay "(" <> join x <> ys <> SOkay ")"
  where
    ys = foldr ((<>) . (SOkay " " <>) . join) (SOkay "") xs

prettySW :: SW String String -> String
prettySW (SOkay x) = x
prettySW (SError es) =
  concatMap (\x -> "\ESC[31;1merror:\ESC[0m " ++ x ++ "\n") es

prettySA :: SA -> String
prettySA (SInt x) = show x
prettySA (SStr x) = show x
prettySA (SSym x) = x

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data SimpleC = SimpleC
  { symbols :: HML.HashMap String Simple,
    builtin :: HML.HashMap String Simplifier,
    inner :: Simple
  }

type Simplifier = SimpleC -> IO Simple

-------------------------------------------------------------------------------

runSimple :: Simplifier
runSimple scs = case out (inner scs) of
  -- user defined symbol.
  SF (SOkay (In (SA (SSym f)))) xs
    | HML.member f (symbols scs) ->
        todo
  -- builtin symbol.
  SF (SOkay (In (SA (SSym f)))) xs
    | HML.member f (builtin scs) ->
        todo
  -- partial evaluation. (ie applying a lambda function)
  SF (SOkay (In (SF x xs))) as -> todo
  -- user defined symbol.
  SA (SSym c) -> case HML.lookup c (symbols scs) of
    Just r -> pure r
    Nothing -> pure $ ssym c
  -- cannot simplify.
  x -> pure $ In x

-- if (SF(SF x xs) as) then eval (SF x xs) with (as) added to symbols. (not quite)
-- runSimple (In (SF (SOkay (In (SA (SSym f)))) xs)) c | member f c = todo
-- runSimple x _ = pure x

-- runFunc :: String -> [SW String Simple] -> IO Simple
-- runFunc = todo

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main _ = do
  testMsg 1
  putStrLn $
    prettySimple $
      sf
        (ssym "do")
        [ sf
            (ssym "print")
            [ sf
                (ssym "add")
                [ sint 5,
                  sf
                    (ssym "mul")
                    [sint 2, sint 7]
                ]
            ],
          sf
            (ssym "print")
            [ sf
                (ssym "List")
                [sint 5, sint 10, sint (-30)]
            ],
          sf
            (ssym "print")
            [ sstr "Hello world!"
            ]
        ]
  testMsg 2
  putStrLn $
    prettySimple $
      sf
        (serr "test error!")
        [sint 5, serr "different test error!"]
  where
    testMsg :: Int -> IO ()
    testMsg x = putStrLn $ "\n\ESC[36;1m[Test " ++ show x ++ "]\ESC[0m"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
