-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

module SLang.Main where

-------------------------------------------------------------------------------

import Data.HashMap.Lazy (HashMap)
import Common

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

defaultArgs :: [String]
defaultArgs = ["~/dev/hsk/test0/app/SLang/test.sl"]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

main :: [String] -> IO ()
main [] = 
  putStrLn $
  prettyExpr $
  beExpr $
  In $ ExprApply 
    (In $ ExprLambda 
      (In $ ExprSym "x")
      (In $ ExprLambda 
        (In $ ExprSym "y")
        (In $ ExprSym "x")
      ))
    (In $ ExprInt 5)
main _ = putError ("invalid args")

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data Program = Program
  { programID :: ProgramID,
  --   programTypes :: HashMap String (SType w),
  --   programClasses :: HashMap String (),
    programFunctions :: HashMap String Expr
  }

-------------------------------------------------------------------------------

data ProgramID = ProgramID
  { programName :: String,
    programMajor :: Int,
    programMinor :: Int,
    programPatch :: Int
  }
  deriving (Show)

-------------------------------------------------------------------------------

data ExprF a
  = ExprUnit
  | ExprInt Int
  | ExprStr String
  | ExprSym String
  | ExprLambda a a
  | ExprApply a a
  | ExprLet String a
  deriving (Functor, Show, Eq)

type Expr = Term ExprF

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- TODO: change paramorphism in order to handle precedence?
prettyExpr :: Expr -> String
prettyExpr = cata prettyExprF

prettyExprF :: ExprF String -> String
prettyExprF (ExprUnit) = "()"
prettyExprF (ExprInt x) = show x 
prettyExprF (ExprStr x) = show x
prettyExprF (ExprSym x) = x
prettyExprF (ExprLambda x y) = "(" ++ x ++ ") -> (" ++ y ++ ")"
prettyExprF (ExprApply x y) = x ++ "(" ++ y ++ ")"
prettyExprF (ExprLet x y) = "let " ++ x ++ " = " ++ y

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- thinking about this more, catamorphisms are not suitable for this kind of
-- beta reduction.
beExpr :: Expr -> Expr
beExpr = cata beExprF

beExprF :: ExprF Expr -> Expr
beExprF (ExprApply f _x) =
  case out f of
    ExprSym _fs -> todo
    ExprLambda _p _b -> todo
    ExprApply _f' _f'x -> todo
    _ -> error "invalid"
beExprF x = In x

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
{------------------------------------------------------------------------------

S    = "((sl.0.0))\n" TopL ("\n" TopL)* eof
TopL = Import | FuncType | FuncDecl

Import   = dimport path
FuncType = funcName "::" Type
FuncDecl = funcName "=" Expr

Type = "int" | "string" | TypeList | TypeArrow | TypeApply
TypeList = "[" Type "]"
TypeArrow = Type "->" Type
TypeApply = typeName "(" (Type)* ")"
-- precendence problem!

------------------------------------------------------------------------------}
-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
