-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Calc.Numeric.Main where

-------------------------------------------------------------------------------

import Control.Monad ((<=<))
import Data.Char (isNumber)
import GHC.Base (when)
import GHC.Float (expFloat, logFloat, powerFloat)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: IO ()
main = do
  x <- getLine
  when (x /= "q") $ do
    putStr "= "
    case exec x of
      Just y -> print y
      Nothing -> putStrLn "invalid input"
    main

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

exec :: String -> Maybe Float
exec = Just . compute <=< parse

-------------------------------------------------------------------------------

-- |
-- __Grammar:__
--
-- > S = AddB
-- >
-- > AddB = SubB ~ ('+' ~ AddB)?
-- > SubB = MulB ~ ('-' ~ SubB)?
-- > MulB = DivB ~ ('*' ~ MulB)?
-- > DivB = PowB ~ ('/' ~ DivB)?
-- > PowB = ParB ~ ('^' ~ PowB)?
-- > ParB = ( '(' ~ S ~ ')' ) | NumB | FunB
-- >
-- > NumB = IntB ~ ('.' ~ IntB)?
-- > IntB = n ~ IntB?
-- >
-- > FunB = SinB | CosB | TanB | EB | LnB
-- > SinB = "sin(" ~ S ~ ')'
-- > CosB = "cos(" ~ S ~ ')'
-- > TanB = "tan(" ~ S ~ ')'
-- > EB = "e^" ~ ParB
-- > LnB = "ln(" ~ S ~ ')'
parse :: String -> Maybe AST
parse s = case pT s of
  Valid i [] -> Just i
  _ -> Nothing
  where
    pT = pA

    pA = (pS ~? ('+' ~> pA)) id AAdd
    pS = (pM ~? ('-' ~> pS)) id ASub
    pM = (pD ~? ('*' ~> pM)) id AMul
    pD = (pP ~? ('/' ~> pD)) id ADiv
    pP = (pB ~? ('^' ~> pP)) id APow
    pB = ('(' ~> pT <~ ')') ~|~ pN ~|~ pF ~|~ pPI
    pN = (pI ~? ('.' ~> pI)) (ANum . read) (\a b -> (ANum . read) (a ++ '.' : b))

    pF = pSin ~|~ pCos ~|~ pTan ~|~ pE ~|~ pLn
      where
        pSin = ("sin(" >~> (pT <~ ')')) ASin
        pCos = ("cos(" >~> (pT <~ ')')) ACos
        pTan = ("tan(" >~> (pT <~ ')')) ATan
        pE = ("e^" >~> pB) AE
        pLn = ("ln(" >~> (pT <~ ')')) ALn

    pPI = (??) "pi" API

    pI (a : e) | isNumber a =
      case pI e of
        Valid b e' -> Valid (a : b) e'
        Invalid -> Valid [a] e
    pI _ = Invalid

-------------------------------------------------------------------------------

-- |
-- > a ~? b
-- > if a is valid:
-- > |  if b is valid:
-- > |  | g a b
-- > |  else:
-- > |  | f a
-- > else:
-- > |  invalid
--
-- valid reads consume 's'
(~?) :: (String -> Grammar a) -> (String -> Grammar b) -> (a -> c) -> (a -> b -> c) -> String -> Grammar c
(~?) a b f g s =
  case a s of
    Valid a' s' -> case b s' of
      Valid b' s'' -> Valid (g a' b') s''
      Invalid -> Valid (f a') s'
    Invalid -> Invalid

-- |
-- 'a' followed by 'b'
--
-- valid reads consume 's'
(~>) :: Char -> (String -> Grammar a) -> String -> Grammar a
(~>) a b (x : s) | x == a = b s
(~>) _ _ _ = Invalid

-- |
-- 'a' followed by 'b'
--
-- valid reads consume 's'
(<~) :: (String -> Grammar a) -> Char -> String -> Grammar a
(<~) a b s = case a s of
  Valid a' s' -> case s' of
    (x : s'') | x == b -> Valid a' s''
    _ -> Invalid
  Invalid -> Invalid

(~|~) :: (String -> Grammar a) -> (String -> Grammar a) -> String -> Grammar a
(~|~) a b s = case a s of
  Valid a' s' -> Valid a' s'
  Invalid -> b s

(>~>) :: String -> (String -> Grammar a) -> (a -> b) -> String -> Grammar b
(>~>) a b f s =
  let (a', b') = splitAt (length a) s
   in if a == a'
        then case b b' of
          Valid b'' b''' -> Valid (f b'') b'''
          _ -> Invalid
        else Invalid

(??) :: String -> a -> String -> Grammar a
(??) x y s =
  let (x', s') = splitAt (length x) s
   in if x == x'
        then Valid y s'
        else Invalid

data Grammar a
  = Valid a String
  | Invalid

-------------------------------------------------------------------------------

compute :: AST -> Float
compute (ANum x) = x
compute (AAdd a b) = compute a + compute b
compute (ASub a b) = compute a - compute b
compute (AMul a b) = compute a * compute b
compute (ADiv a b) = compute a / compute b
compute (APow a b) = powerFloat (compute a) (compute b)
compute (ASin a) = sin (compute a)
compute (ACos a) = cos (compute a)
compute (ATan a) = tan (compute a)
compute (AE a) = expFloat (compute a)
compute (ALn a) = logFloat (compute a)
compute API = pi

-------------------------------------------------------------------------------

data AST
  = ANum Float
  | AAdd AST AST
  | ASub AST AST
  | AMul AST AST
  | ADiv AST AST
  | APow AST AST
  | ASin AST
  | ACos AST
  | ATan AST
  | AE AST
  | ALn AST
  | API

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- parse = Just . fst <=< parseTop
--   where
--   parseTop = parseAdd

--   parseAdd = ap parseAdd parseSub '+' AAdd
--   parseSub = ap parseSub parseMul '-' ASub
--   parseMul = ap parseMul parseDiv '*' AMul
--   parseDiv = ap parseDiv parsePar '/' ADiv

--   parsePar ('(' : s) = do
--     (i, e) <- parseTop s
--     case e of
--       (')' : e') -> Just (i, e')
--       _ -> Nothing
--   parsePar s = parseNum s

--   parseNum s = do
--     (a, e) <- parseInt s
--     case e of
--       ('.' : e') -> do
--         (b, e'') <- parseInt e'
--         Just ((ANum . read) (a ++ '.' : b), e'')
--       _ -> Just ((ANum . read) a, e)

--   parseInt (a : e) | isNumber a =
--     case parseInt e of
--       Just (b, e') -> Just (a : b, e')
--       Nothing -> Just ([a], e)
--   parseInt _ = Nothing

--   ap ::
--     (String -> Maybe (AST, String)) ->
--     (String -> Maybe (AST, String)) ->
--     Char ->
--     (AST -> AST -> AST) ->
--     (String -> Maybe (AST, String))
--   ap self next sep into s = do
--     (a, e) <- next s
--     case e of
--       x : e' | x == sep -> do
--         (b, e'') <- self e'
--         Just (into a b, e'')
--       _ -> Just (a, e)