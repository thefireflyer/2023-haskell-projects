module SLang.DeBruijn where

import Control.Monad
import SLang.Pretty
import Test.QuickCheck (Arbitrary (arbitrary), Gen, applyArbitrary2, applyArbitrary3, applyArbitrary4, arbitrary, oneof)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- | Term info.
data TI
  = TI
      { tmLine :: Int,
        tmCol :: Int
      }
  | TNA
  deriving (Show, Eq)

-- | Types.
data Ty
  = TyArr Ty Ty
  | TyTuple Ty [Ty]
  | TyVariant (String, Ty) [(String, Ty)]
  | TyList Ty
  | TyUnit
  | TyBool
  | TyInt
  | TyString
  deriving (Show, Eq)

-- | Term.
data Tm
  = -- Core terms.
    TmVar TI Int Int
  | TmAbs TI (Maybe String) Ty Tm
  | TmApp TI Tm Tm
  | -- Control flow extensions.
    TmFix TI Tm
  | TmBEq TI Tm Tm
  | TmPrint TI Tm
  | TmShow TI Tm
  | TmRead TI Tm Ty
  | TmError TI String Ty
  | -- Product terms.
    TmTuple TI Tm [Tm]
  | TmProj TI Tm Int
  | -- Sum terms.
    TmVariant TI String Tm Ty
  | TmCaseOf TI Tm [(String, Tm)] (Either (String, Tm) Tm)
  | -- List terms.
    TmNil TI Ty
  | TmCons TI Tm Tm
  | -- Base terms.
    TmUnit TI
  | TmBool TI Bool
  | TmInt TI Int
  | TmString TI String
  | -- Syntax sugar.
    TmSeq TI Tm [Tm]
  | TmLet TI String Tm Tm
  | TmLetRec TI String Ty Tm Tm
  | TmIsNil TI Tm
  | TmHead TI Tm
  | TmTail TI Tm
  | TmIf TI Tm Tm Tm
  | TmAdd TI Tm Tm
  | TmSub TI Tm Tm
  | TmMul TI Tm Tm
  | TmDiv TI Tm Tm
  | TmRem TI Tm Tm
  deriving (Show, Eq)

tmVal :: Tm -> Bool
tmVal (TmAbs {}) = True
tmVal (TmTuple _ t1 ts) = tmVal t1 && all tmVal ts
tmVal (TmVariant _ _ t1 _) = tmVal t1
tmVal (TmNil {}) = True
tmVal (TmCons _ t1 t2) = tmVal t1 && tmVal t2
tmVal (TmUnit {}) = True
tmVal (TmBool {}) = True
tmVal (TmInt {}) = True
tmVal (TmString {}) = True
tmVal _ = False

tmBaseIsh :: Tm -> Bool
tmBaseIsh (TmVar {}) = True
tmBaseIsh (TmNil {}) = True
tmBaseIsh (TmUnit {}) = True
tmBaseIsh (TmBool {}) = True
tmBaseIsh (TmInt {}) = True
tmBaseIsh (TmString {}) = True
tmBaseIsh _ = False

tmBase :: Tm -> Bool
tmBase (TmUnit {}) = True
tmBase (TmBool {}) = True
tmBase (TmInt {}) = True
tmBase (TmString {}) = True
tmBase _ = False

wrapApp :: Tm -> Tm -> Tm
wrapApp = TmApp TNA

getListTy :: Tm -> Ty
getListTy (TmCons _ _ t2) = getListTy t2
getListTy (TmNil _ ty) = ty
getListTy _ = error "???"

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

type NameCtx = [String]

addName :: NameCtx -> String -> NameCtx
addName ctx name = name : ctx

getName :: NameCtx -> Int -> String
getName ctx x = if x < length ctx then ctx !! x else "ukn" <> show (x - length ctx)

newName :: NameCtx -> String -> (NameCtx, String)
newName ctx x =
  if x `elem` ctx
    then let x' = x <> show (length ctx + 1) in (x' : ctx, x')
    else (x : ctx, x)

-------------------------------------------------------------------------------

prTI :: TI -> String
prTI (TI line col) = "line " <> show line <> ":" <> show col
prTI TNA = ""

prVr :: Int -> (String, Ty) -> String
prVr d (x, t) = x <> ":" <> prTy d t

prTy :: Int -> Ty -> String
prTy d (TyArr ty1 ty2) = paren d $ prTy (d + 1) ty1 <> " → " <> prTy (d + 1) ty2
prTy d (TyTuple ty tys) =
  curly d $ prTy (d + 1) ty <> concatMap ((", " <>) . prTy (d + 1)) tys
prTy d (TyVariant vr vrs) =
  between "⟨" "⟩" d $ prVr (d + 1) vr <> concatMap ((" | " <>) . prVr (d + 1)) vrs
prTy d (TyList ty) = squar d $ prTy (d + 1) ty
prTy d TyUnit = paren d ""
prTy _ TyBool = ansiCl 4 "Bool"
prTy _ TyInt = ansiCl 4 "Int"
prTy _ TyString = ansiCl 4 "String"

-- ansiCl 189

-- prCons :: NameCtx -> T -> String
-- prCons ctx t = case out t of
--   TmCons _ t1 t2 -> ", "<>prTm ctx t1<>prCons ctx t2
--   TmNil _ _ -> "]"
--   _ -> "] <> "<>prTm ctx t

prTm :: NameCtx -> Tm -> String
prTm ctx = prTm_ ctx 0 0

prTm_ :: NameCtx -> Int -> Int -> Tm -> String
prTm_ ctx d p t =
  let inner d p = case t of
        TmVar _ _ n
          | length ctx /= n ->
              error $ "Invalid Index (" <> show (length ctx) <> " ≠ " <> show n <> ")"
        TmVar _ x n | x >= n -> ansiBl 88 $ getName ctx x
        TmVar _ x _ -> getName ctx x
        TmAbs _ Nothing ty t1 ->
          "λ_:" <> prTy (d + 1) ty <> ". " <> prTm_ ctx (d + 1) p t1
        TmAbs _ (Just x) ty t1 ->
          let (ctx', x') = newName ctx x
           in "λ"
                <> x'
                <> ":"
                <> prTy (d + 1) ty
                <> ". "
                <> prTm_ ctx' (d + 1) p t1
        TmApp _ t1 t2 -> prTm_ ctx (d + 1) p t1 <> " " <> prTm_ ctx (d + 1) p t2
        TmFix _ t1 -> "fix " <> prTm_ ctx (d + 1) p t1
        TmBEq _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " == " <> prTm_ ctx (d + 1) p t2
        TmPrint _ t1 -> "print " <> prTm_ ctx (d + 1) p t1
        TmShow _ t1 -> "show " <> prTm_ ctx (d + 1) p t1
        TmRead _ t1 ty -> "read " <> prTm_ ctx (d + 1) p t1 <> " as " <> prTy (d + 1) ty
        TmError _ s ty1 -> "error " <> show s <> " as " <> prTy (d + 1) ty1
        TmTuple _ t1 ts ->
          prTm_ ctx (d + 1) p t1
            <> concatMap ((", " <>) . prTm_ ctx (d + 1) p) ts
        TmProj _ t1 i -> prTm_ ctx (d + 1) p t1 <> "." <> show i
        TmVariant _ vr t1 ty ->
          vr
            <> " "
            <> prTm_ ctx (d + 1) p t1
            <> ansiCl 104 " as "
            <> prTy (d + 1) ty
        TmCaseOf _ t1 cs (Left c) ->
          ansiCl 5 "case "
            <> prTm_ ctx (d + 1) p t1
            <> ansiCl 5 " of\n"
            <> concatMap
              ( \(vr, vt) ->
                  concatMap (const "  ") [0 .. d]
                    <> "| "
                    <> vr
                    <> " → "
                    <> prTm_ ctx (d + 1) p vt
                    <> "\n"
              )
              (cs <> [c])
        TmCaseOf _ t1 cs (Right c) ->
          ansiCl 5 "case "
            <> prTm_ ctx (d + 1) p t1
            <> ansiCl 5 " of\n"
            <> concatMap
              ( \(vr, vt) ->
                  concatMap (const "  ") [0 .. d]
                    <> "| "
                    <> vr
                    <> " → "
                    <> prTm_ ctx (d + 1) 0 vt
                    <> "\n"
              )
              cs
            <> concatMap (const "  ") [0 .. d]
            <> "| _ → "
            <> prTm_ ctx (d + 1) 0 c
            <> "\n"
            <> concatMap (const "  ") [1 .. d]
        TmNil _ ty -> "nil" <> squar d (prTy (d + 1) ty)
        TmCons _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " :: " <> prTm_ ctx (d + 1) p t2
        TmUnit _ -> paren d ""
        TmBool _ x -> show x -- ansiCl 203 (show x)
        TmInt _ x -> ansiCl 193 (show x) -- ansiCl 198 (show x)
        TmString _ x -> ansiCl 216 (show x)
        TmSeq _ x xs ->
          ansiCl 5 "do\n"
            <> concatMap (const "  ") [0 .. d]
            <> prTm_ ctx (d + 1) 0 x
            <> concatMap
              ( ( ( "\n"
                      <> concatMap (const "  ") [0 .. d]
                  )
                    <>
                )
                  . prTm_ ctx (d + 1) 0
              )
              xs
        TmLet _ x t1 t2 ->
          let (ctx', x') = newName ctx x
           in ansiCl 141 "let "
                <> x'
                <> " = "
                <> prTm_ ctx (d + 1) p t1
                <> ansiCl 141 " in\n"
                <> concatMap (const "  ") [0 .. d]
                <> prTm_ ctx' (d + 1) 0 t2
        TmLetRec _ x ty1 t1 t2 ->
          let (ctx', x') = newName ctx x
           in ansiCl 141 "let rec "
                <> x'
                <> ":"
                <> prTy (d + 1) ty1
                <> " = "
                <> prTm_ ctx' (d + 1) p t1
                <> ansiCl 141 " in\n"
                <> concatMap (const "  ") [0 .. d]
                <> prTm_ ctx' (d + 1) 0 t2
        TmIf _ t1 t2 t3 ->
          ansiCl 5 "if "
            <> prTm_ ctx (d + 1) 0 t1
            <> "\n"
            <> concatMap (const "  ") [0 .. d]
            <> ansiCl 5 "then "
            <> prTm_ ctx (d + 1) 0 t2
            <> "\n"
            <> concatMap (const "  ") [0 .. d]
            <> ansiCl 5 "else "
            <> prTm_ ctx (d + 1) 0 t3
            <> "\n"
            <> concatMap (const "  ") [1 .. d]
        TmAdd _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " + " <> prTm_ ctx (d + 1) p t2
        TmSub _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " - " <> prTm_ ctx (d + 1) p t2
        TmMul _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " * " <> prTm_ ctx (d + 1) p t2
        TmDiv _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " / " <> prTm_ ctx (d + 1) p t2
        TmRem _ t1 t2 ->
          prTm_ ctx (d + 1) p t1 <> " % " <> prTm_ ctx (d + 1) p t2
        _ -> error $ "TODO: prTm not implemented for " <> show t
   in if tmBaseIsh t
        then inner d (p + 1)
        else paren d $ inner d (p + 1)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

testDeBruijn :: IO ()
testDeBruijn = do
  -- Test prTy
  putStrLn "[Testing prTy]"
  putStrLn . prTy 0 $ TyArr TyUnit TyBool
  putStrLn . prTy 0 $ TyTuple TyUnit [TyBool, TyInt, TyString, TyArr TyInt TyBool]
  putStrLn . prTy 0 $
    TyVariant ("Some", TyInt) [("None", TyUnit), ("Maybe", TyBool)]
  putStrLn . prTy 0 $ TyList (TyTuple TyString [TyBool])

  -- Test prTm
  putStrLn "\n[Testing prTm]"
  putStrLn . prTm [] $ TmAbs TNA Nothing TyUnit (TmUnit TNA)
  putStrLn . prTm [] $ TmAbs TNA (Just "x") TyUnit (TmVar TNA 0 1)
  putStrLn . prTm [] $
    TmApp
      TNA
      ( TmAbs
          TNA
          (Just "x")
          (TyArr TyUnit TyUnit)
          ( TmAbs
              TNA
              (Just "x")
              TyUnit
              (TmVar TNA 1 2)
          )
      )
      ( TmAbs
          TNA
          (Just "y")
          TyUnit
          (TmVar TNA 0 1)
      )
  putStrLn . prTm [] $
    TmApp
      TNA
      ( TmAbs
          TNA
          (Just "x")
          TyString
          ( TmBEq
              TNA
              (TmVar TNA 0 1)
              (TmString TNA "apples")
          )
      )
      (TmString TNA "oranges")
  putStrLn . prTm [] $
    TmPrint
      TNA
      (TmString TNA "hello world!")
  putStrLn . prTm [] $
    TmApp
      TNA
      ( TmAbs
          TNA
          (Just "x")
          (TyTuple TyInt [TyBool])
          ( TmProj
              TNA
              (TmVar TNA 0 1)
              1
          )
      )
      ( TmTuple
          TNA
          (TmInt TNA 5)
          [TmBool TNA True]
      )
  putStrLn . prTm [] $
    TmAbs
      TNA
      (Just "x")
      TyString
      ( TmCaseOf
          TNA
          ( TmVariant
              TNA
              "Some"
              (TmInt TNA 5)
              (TyVariant ("Some", TyInt) [("None", TyUnit)])
          )
          [ ( "Some",
              TmAbs
                TNA
                (Just "x")
                TyInt
                (TmString TNA "wowza!")
            )
          ]
          ( Left
              ( "None",
                TmAbs
                  TNA
                  (Just "y")
                  TyUnit
                  (TmVar TNA 1 2)
              )
          )
      )
  putStrLn . prTm [] $
    TmAbs
      TNA
      (Just "x")
      TyString
      ( TmCaseOf
          TNA
          ( TmVariant
              TNA
              "Some"
              (TmInt TNA 5)
              (TyVariant ("Some", TyInt) [("None", TyUnit)])
          )
          [ ( "Some",
              TmAbs
                TNA
                (Just "x")
                TyInt
                (TmString TNA "wowza!")
            )
          ]
          (Right (TmVar TNA 0 1))
      )
  putStrLn . prTm [] $
    TmPrint
      TNA
      ( TmTuple
          TNA
          (TmString TNA "hello world!")
          [ TmNil TNA TyBool,
            TmCons
              TNA
              (TmInt TNA 5)
              (TmNil TNA TyInt),
            TmCons
              TNA
              (TmInt TNA 7)
              ( TmCons
                  TNA
                  (TmInt TNA 8)
                  (TmNil TNA TyInt)
              )
          ]
      )
  putStrLn . prTm [] $
    TmAbs
      TNA
      (Just "x")
      (TyList TyBool)
      ( TmCons
          TNA
          (TmBool TNA True)
          ( TmCons
              TNA
              (TmBool TNA False)
              ( TmCons
                  TNA
                  (TmVar TNA 0 1)
                  (TmVar TNA 0 1)
              )
          )
      )
  putStrLn . prTm [] $
    TmSeq
      TNA
      ( TmLet
          TNA
          "x"
          (TmInt TNA 5)
          ( TmIf
              TNA
              (TmBEq TNA (TmVar TNA 0 1) (TmInt TNA 3))
              (TmPrint TNA (TmString TNA "x=3"))
              (TmPrint TNA (TmString TNA "x ≠ 3"))
          )
      )
      [ TmPrint TNA (TmBool TNA False),
        TmPrint TNA (TmString TNA "Maybe?")
      ]
  putStrLn . prTm [] $
    TmAbs
      TNA
      (Just "x")
      TyString
      ( TmCaseOf
          TNA
          ( TmVariant
              TNA
              "Some"
              (TmInt TNA 5)
              (TyVariant ("Some", TyInt) [("None", TyUnit)])
          )
          [ ( "Some",
              TmAbs
                TNA
                (Just "x")
                TyInt
                (TmString TNA "wowza!")
            )
          ]
          ( Right
              ( TmSeq
                  TNA
                  ( TmLet
                      TNA
                      "x"
                      (TmInt TNA 5)
                      ( TmIf
                          TNA
                          (TmBEq TNA (TmVar TNA 0 2) (TmInt TNA 3))
                          (TmPrint TNA (TmString TNA "x=3"))
                          (TmPrint TNA (TmString TNA "x ≠ 3"))
                      )
                  )
                  [ TmPrint TNA (TmBool TNA False),
                    TmPrint TNA (TmString TNA "Maybe?"),
                    TmString TNA "apples!"
                  ]
              )
          )
      )
  putStrLn . prTm [] $ TmVar TNA 0 0
  putStrLn . prTm [] $
    TmAbs
      TNA
      (Just "x")
      TyUnit
      (TmVar TNA 2 1)
  putStrLn . prTm [] $ tmFac 5
  putStrLn . prTm [] $ tmFacR 5

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

-- either (foldMap putStrLn) (putStrLn . prTy) . tyOf [] [] .
--   In $ TmApp TNA
--     (TmAbs TNA (Just "x") (TyArr TyUnit TyUnit)
--       (TmAbs TNA (Just "x") TyUnit
--         (TmVar TNA 1 2)))
--     (TmAbs TNA (Just "y") TyUnit
--       (TmVar TNA 0 1))
-- either (foldMap putStrLn) (putStrLn . prTy) . tyOf [] [] .
--   In $ TmSeq TNA
--   (TmLet TNA "x"
--     (TmInt TNA 5)
--     (TmIf TNA
--       (TmBEq TNA (TmVar TNA 0 1) (TmInt TNA 3))
--       (TmPrint TNA (TmString TNA "x=3"))
--       (TmString TNA "x ≠ 3")))
--   [In $ TmPrint TNA (TmBool TNA False)
--   ,In $ TmPrint TNA (TmString TNA "Maybe?")]
-- either (foldMap putStrLn) (putStrLn . prTy) . tyOf [] [] .
--   In $ TmApp TNA
--   (TmAbs TNA (Just "x") (TyTuple TyInt [TyBool])
--     (TmProj TNA
--       (TmVar TNA 0 1)
--       1))
--   (TmTuple TNA
--     (TmInt TNA 5)
--     [In $ TmBool TNA True])
-- either (foldMap putStrLn) (putStrLn . prTy) . tyOf [] [] .
--   In $ TmVariant TNA "Some"
--       (TmInt TNA 5)
--       (TyVariant ("Some", TyInt) [("None", TyUnit)])

-- either (foldMap putStrLn) (putStrLn . prTy) . tyOf [] [] .
--   In $ TmAbs TNA (Just "x") TyString
--   (TmCaseOf TNA
--     (TmVariant TNA "Some"
--       (TmInt TNA 5)
--       (TyVariant ("Some", TyInt) [("None", TyUnit)]))
--     [("Some", In $ TmAbs TNA (Just "x") TyInt
--       (TmString TNA "wowza!"))
--     ,("None", In $ TmAbs TNA (Just "y") TyUnit
--       (TmVar TNA 1 2))]
--     Nothing)

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

instance Arbitrary TI where
  arbitrary :: Gen TI
  arbitrary = liftM2 TI arbitrary arbitrary

instance Arbitrary Ty where
  arbitrary :: Gen Ty
  arbitrary =
    oneof
      [ liftM2 TyArr arbitrary arbitrary,
        liftM2 TyTuple arbitrary arbitrary,
        liftM2 TyVariant arbitrary arbitrary,
        TyList <$> arbitrary,
        pure TyUnit,
        pure TyBool,
        pure TyInt,
        pure TyString
      ]

instance Arbitrary Tm where
  arbitrary :: Gen Tm
  arbitrary =
    oneof
      [ TmUnit <$> arbitrary,
        applyArbitrary2 TmBool,
        applyArbitrary2 TmInt,
        applyArbitrary2 TmString,
        applyArbitrary3 TmVar,
        applyArbitrary4 TmAbs,
        applyArbitrary3 TmApp,
        applyArbitrary2 TmFix,
        applyArbitrary3 TmBEq,
        applyArbitrary2 TmPrint,
        applyArbitrary3 TmTuple,
        applyArbitrary3 TmProj,
        applyArbitrary4 TmVariant,
        applyArbitrary4 TmCaseOf,
        applyArbitrary2 TmNil,
        applyArbitrary3 TmCons,
        applyArbitrary3 TmSeq,
        applyArbitrary4 TmLet,
        TmLetRec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        applyArbitrary2 TmIsNil,
        applyArbitrary2 TmHead,
        applyArbitrary2 TmTail,
        applyArbitrary4 TmIf,
        applyArbitrary3 TmAdd,
        applyArbitrary3 TmSub,
        applyArbitrary3 TmMul,
        applyArbitrary3 TmDiv,
        applyArbitrary3 TmRem
      ]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

tmFac :: Int -> Tm
tmFac x =
  TmLet
    TNA
    "fac"
    ( TmFix
        TNA
        ( TmAbs
            TNA
            (Just "fac")
            (TyArr TyInt TyInt)
            ( TmAbs
                TNA
                (Just "x")
                TyInt
                ( TmCaseOf
                    TNA
                    (TmVar TNA 0 2)
                    [ ( "Succ",
                        TmAbs
                          TNA
                          (Just "x")
                          TyInt
                          ( TmMul
                              TNA
                              (TmVar TNA 1 3)
                              ( TmApp
                                  TNA
                                  (TmVar TNA 2 3)
                                  (TmVar TNA 0 3)
                              )
                          )
                      )
                    ]
                    (Right (TmInt TNA 1))
                )
            )
        )
    )
    ( TmApp
        TNA
        (TmVar TNA 0 1)
        (TmInt TNA x)
    )

tmFacR :: Int -> Tm
tmFacR x =
  TmLetRec
    TNA
    "fac"
    (TyArr TyInt TyInt)
    ( TmAbs
        TNA
        (Just "x")
        TyInt
        ( TmIf
            TNA
            ( TmBEq
                TNA
                (TmVar TNA 0 2)
                (TmInt TNA 0)
            )
            (TmInt TNA 1)
            ( TmMul
                TNA
                (TmVar TNA 0 2)
                ( TmApp
                    TNA
                    (TmVar TNA 1 2)
                    ( TmSub
                        TNA
                        (TmVar TNA 0 2)
                        (TmInt TNA 1)
                    )
                )
            )
        )
    )
    ( TmLet
        TNA
        "res"
        ( TmApp
            TNA
            (TmVar TNA 0 1)
            (TmInt TNA x)
        )
        ( TmSeq
            TNA
            (TmPrint TNA (TmString TNA "\n"))
            [ TmPrint TNA (TmShow TNA (TmInt TNA x)),
              TmPrint TNA (TmString TNA "! = "),
              TmPrint TNA (TmShow TNA (TmVar TNA 0 2)),
              TmPrint TNA (TmString TNA "\n"),
              TmVar TNA 0 2
            ]
        )
    )
