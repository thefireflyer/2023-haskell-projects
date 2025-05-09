-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module Calc.Algebraic.Main where

-------------------------------------------------------------------------------

import Common (todo)
import Control.Monad ((<=<))
import GHC.Base (when)
import GHC.Float (powerFloat)
import Text.Parsec
import Prelude hiding (pred)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: IO ()
main = do
  x <- getLine
  when (x /= "q") $ do
    case exec x of
      Left y -> putStrLn ("Error: " ++ show y ++ "\n")
      Right y -> putStrLn ("= " ++ show y ++ "\n")
    main

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

exec :: String -> Either ParseError [AST]
exec = Right . map (symbolic . numeric) <=< parseMath

-- exec = sat . symbolic . numeric <=< parseMath

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- |
-- __Grammar:__
--
-- > S = Stmt
-- >
-- > Stmt = Pred ~ (';' ~ Stmt)?
-- > Pred = Relv  ~ ('|' ~ Pred)?
-- > Relv = A/S ~ (Rel ~ Relv)?
-- >
-- > A/S  = M/D  ~ ( ( '+' | '-' ) ~ A/S )?
-- > M/D  = Pow  ~ ( ( '*' | '/' ) ~ M/D )?
-- > Pow  = Unit ~ ('^' ~ Unit)?
-- >
-- > Unit = ('-')? ~ (Par | Vec | Num | Fun | Sym) ~ ('!')?
-- >
-- > Par = '(' ~ Relv ~ ')'
-- > Vec = '[' ~ Relv ~ (',' ~ Relv)* ~ ']'
-- >
-- > Rel = "=" | ">=" | "<=" | ">" | "<" | "!="
-- >
-- > Num = Int ~ ('.' ~ Int)?
-- > Int = n ~ Int?
-- >
-- > Fun = Sin | Cos | Tan | E | Ln
-- >
-- > Sin = "sin(" ~ S ~ ')'
-- > Cos = "cos(" ~ S ~ ')'
-- > Tan = "tan(" ~ S ~ ')'
-- >
-- > E = "e^" ~ Unit
-- > Ln = "ln(" ~ S ~ ')'
-- >
-- > Sym = (alpha | '_') ~ Sym?
parseMath :: String -> Either ParseError [AST]
parseMath = parse s "(unknown)"
  where
    s = stmt
    stmt = sepBy pred (string "; ")
    pred = chainl1 relv (do _ <- char '|'; return (Bin Pred))
    relv = chainl1 expr rel
    expr = chainl1 term add_sub
    term = chainl1 expo mul_div
    expo = chainr1 factor (do _ <- char '^'; return (Bin Pow))
    factor =
      try
        ( do
            _ <- char '('
            x <- pred
            _ <- char ')'
            return x
        )
        <|> try
          ( do
              _ <- char '['
              xs <- sepBy1 pred (string ", ")
              _ <- char ']'
              return (Vec xs)
          )
        <|> try (Num . read <$> many1 digit)
        <|> try (Sym <$> many1 letter)

    rel =
      try (do _ <- string "="; return (Bin Eqq))
        <|> try (do _ <- string ">="; return (Bin Geq))
        <|> try (do _ <- string "<="; return (Bin Leq))
        <|> try (do _ <- string ">"; return (Bin Gtt))
        <|> try (do _ <- string "<"; return (Bin Ltt))
        <|> try (do _ <- string "!="; return (Bin Neq))

    add_sub = try (string "+" >> return (Bin Add)) <|> try (string "-" >> return (Bin Sub))
    mul_div = try (string "*" >> return (Bin Mul)) <|> try (string "/" >> return (Bin Div))

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | Reduces every, purely numeric, expression into a number.
numeric :: AST -> AST
numeric (Bin op a b) =
  let a' = numeric a
      b' = numeric b
   in case (a', b') of
        (Num a'', Num b'') -> case op of
          Pred -> error "undefined"
          Eqq -> Boo (a'' == b'')
          Geq -> Boo (a'' >= b'')
          Leq -> Boo (a'' <= b'')
          Gtt -> Boo (a'' > b'')
          Ltt -> Boo (a'' < b'')
          Neq -> Boo (a'' /= b'')
          Add -> Num (a'' + b'')
          Sub -> Num (a'' - b'')
          Mul -> Num (a'' * b'')
          Div -> Num (a'' / b'')
          Pow -> Num (powerFloat a'' b'')
        _ -> Bin op a' b'
numeric (Vec xs) = Vec (map numeric xs)
numeric x = x

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

symbolic :: AST -> AST
symbolic (Bin op a b) =
  let a' = symbolic a
      b' = symbolic b
   in case (a', b') of
        (Num _, Num _) -> numeric (Bin op a' b') -- numeric problems
        (Sym a'', Sym b'') | a'' == b'' -> case op of
          Eqq -> Boo True -- x=x ==> true
          Neq -> Boo False -- x!=x ==> false
          Geq -> Boo True -- x>=x ==> true
          Leq -> Boo True -- x<=x ==> true
          Gtt -> Boo False -- x>x ==> false
          Ltt -> Boo False -- x<x ==> false
          Add -> Bin Mul (Num 2) (Sym a'') -- x+x ==> 2x
          Sub -> Num 0 -- x-x ==> 0
          Mul -> Bin Pow (Sym a'') (Num 2) -- x*x ==> x^2
          Div -> Num 1 -- x/x ==> 1
          Pow -> Bin Pow a' b' -- x^x ==> x^x
          Pred -> error "x|x --- undefined case"
        (Sym _, Sym _) -> Bin op a' b' -- this is an SAT problem or not reduce-able
        (Boo a'', Boo b'') -> case op of
          Eqq -> Boo (a'' == b'')
          Neq -> Boo (a'' /= b'')
          Geq -> error "not defined for booleans"
          Leq -> error "not defined for booleans"
          Gtt -> error "not defined for booleans"
          Ltt -> error "not defined for booleans"
          Add -> error "not defined for booleans"
          Sub -> error "not defined for booleans"
          Mul -> error "not defined for booleans"
          Div -> error "not defined for booleans"
          Pow -> error "not defined for booleans"
          Pred -> Boo (a'' && b'') -- a|true ==> a --- a|false ==> false
        (Vec a'', Vec b'') -> case op of
          Pred -> error "[...] | [...] is undefined"
          Pow -> error "[...] ^ [...] is undefined"
          op' -> Vec (zipWith (curry f) a'' b'') -- interpret everything as element-by-element
            where
              f (a''', b''') = symbolic (Bin op' a''' b''')
        (Vec a'', Num _) -> case op of -- very inconsistent, figure something else out!!
          Eqq -> Boo False
          Neq -> Boo True
          Geq -> todo -- norm(vec) >= num
          Leq -> todo -- norm(vec) <= num
          Gtt -> todo -- norm(vec) > num
          Ltt -> todo -- norm(vec) < num
          Pred -> error "[...] | number is undefined"
          _ -> Vec (map f a'')
            where
              f = symbolic . Bin op b'
        (Num _, Sym _) -> Bin op a' b' -- SAT problem or not reduce-able
        (Sym _, Num _) -> Bin op a' b' -- SAT problem or not reduce-able
        (_a'', Bin _b_op _b_a _b_b) -> todo -- polynomials and related
        (Bin _a_op _a_a _a_b, _b'') -> todo -- polynomials and related
        (_, _) -> todo
symbolic (Vec xs) = Vec (map symbolic xs)
symbolic (Boo x) = Boo x
symbolic (Num x) = Num x
symbolic (Sym x) = Sym x

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

sat :: a
sat = error "todo"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data AST
  = Bin Op AST AST
  | Num Float
  | Sym String
  | Vec [AST]
  | Boo Bool
  deriving (Show, Eq)

-------------------------------------------------------------------------------

data Op
  = Pred
  | Eqq
  | Geq
  | Leq
  | Gtt
  | Ltt
  | Neq
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show, Eq)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
