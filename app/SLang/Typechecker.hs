module SLang.Typechecker where

import Common
import Data.Functor (($>))
import Data.List (find)
import SLang.DeBruijn
import Test.HUnit
import Test.QuickCheck

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

data TyErr
  = -- | Free variable.
    FreeVar Int
  | -- | Case output type mismatch. (if, ==, case of)
    CaseMs Tm Ty Tm Ty
  | -- | Function input type and application type mismatch.
    AppMs Tm Ty Ty
  | -- | Should be a function.
    ArrMs Tm Ty Tm Ty
  | -- | Out of bounds projection.
    ProjB Tm Ty Int
  | -- | Projection on non-tuple term.
    ProjMs Tm Ty
  | -- | Wrong constructor type for variant.
    VrTyMs Tm Ty String Ty
  | -- | Unknown constructor for variant.
    VrCxMs Tm String Ty
  | -- | Missing case(s) for variant.
    VrDxMs Tm Ty [String]
  | -- | Should be a variant.
    VrMs Tm Ty
  | -- | If-else on non-Bool.
    IfMs Tm Ty
  deriving (Eq, Show)

prTyErr :: TyErr -> String
prTyErr e = case e of
  FreeVar x ->
    "Variable with de Bruijn index "
      <> show x
      <> " is free."
      <> "Cannot typecheck free variables."
  -- CaseMs x1 t1 x2 t2 ->
  --   ""
  -- AppMs x1 t1 t2 -> ""
  -- ArrMs x1 t1 x2 t2 -> ""
  -- ProjB x1 t1 i -> ""
  -- ProjMs x1 t1 -> ""
  -- VrTyMs x1 t1 c vr -> ""
  -- VrCxMs x1 c vr -> ""
  -- VrMs x1 t1 -> ""
  -- IfMs x1 t1 -> ""
  _ -> error ""

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

type TyM = Fallible TyErr Ty

err :: TyErr -> TyM
err = Failed . pure

-------------------------------------------------------------------------------

type TyCtx = [Ty]

addTy :: TyCtx -> Ty -> TyCtx
addTy ctx ty = ty : ctx

getTy :: TyCtx -> Int -> Maybe Ty
getTy ctx x = if x < length ctx then pure (ctx !! x) else Nothing

-------------------------------------------------------------------------------

tyOf :: Tm -> TyM
tyOf = tyOf_ [] []

tyOf_ :: NameCtx -> TyCtx -> Tm -> TyM
tyOf_ ntx ctx tm = case tm of
  -- core terms.
  TmVar _ x _ -> maybe (err . FreeVar $ x - length ntx) pure (getTy ctx x)
  TmAbs _ (Just x) ty1 t1 ->
    let ctx' = addTy ctx ty1
     in let (ntx', _) = newName ntx x
         in TyArr ty1 <$> tyOf_ ntx' ctx' t1
  TmAbs _ Nothing ty1 t1 ->
    let ctx' = addTy ctx ty1
     in TyArr ty1 <$> tyOf_ ntx ctx' t1
  TmApp _ t1 t2 -> do
    (ty1, ty2) <- (,) <$> tyOf_ ntx ctx t1 <*> tyOf_ ntx ctx t2
    case ty1 of
      TyArr ty11 ty12 ->
        if ty2 == ty11
          then pure ty12
          else err $ AppMs t2 ty2 ty11
      _ -> err $ ArrMs t1 ty1 t2 ty2
  -- control flow.
  TmFix _ t1 -> do
    ty1 <- tyOf_ ntx ctx t1
    case ty1 of
      TyArr ty11 ty12 ->
        if ty11 == ty12
          then pure ty12
          else err $ AppMs t1 ty1 (TyArr ty11 ty11)
      _ -> err $ AppMs t1 ty1 (TyArr ty1 ty1) -- bit weird...
  TmBEq _ t1 t2 -> do
    (ty1, ty2) <- (,) <$> tyOf_ ntx ctx t1 <*> tyOf_ ntx ctx t2
    if ty1 == ty2
      then pure TyBool
      else err $ CaseMs t1 ty1 t2 ty2
  TmPrint _ t1 -> do
    ty1 <- tyOf_ ntx ctx t1
    if ty1 == TyString
      then pure TyUnit
      else err $ AppMs t1 ty1 TyString
  TmShow _ t1 -> TyString <$ tyOf_ ntx ctx t1
  TmRead _ t1 tyd -> do
    ty1 <- tyOf_ ntx ctx t1
    if ty1 == TyString
      then pure $ TyVariant ("Some", tyd) [("None", TyUnit)]
      else err $ AppMs t1 ty1 TyString
  TmError _ _ ty1 -> pure ty1
  -- product terms.
  TmTuple _ t1 ts -> do
    ty1 <- tyOf_ ntx ctx t1
    tys <- mapM (tyOf_ ntx ctx) ts
    pure $ TyTuple ty1 tys
  TmProj _ t1 i -> do
    ty1 <- tyOf_ ntx ctx t1
    case ty1 of
      TyTuple tys1 tys ->
        if i == 0
          then pure tys1
          else
            if i - 1 < length tys
              then pure (tys !! (i - 1))
              else err $ ProjB t1 ty1 i
      _ -> err $ ProjMs t1 ty1
  -- sum terms.
  TmVariant _ x t1 ty -> do
    ty1 <- tyOf_ ntx ctx t1
    case ty of
      TyVariant cx1 cxs ->
        case find ((==) x . fst) (cx1 : cxs) of
          Just (_, cty) ->
            if ty1 == cty
              then pure ty
              else err $ VrTyMs t1 ty1 x ty
          Nothing -> err $ VrCxMs t1 x ty
      _ -> err $ VrMs t1 ty
  TmCaseOf _ t1 dxr (Right dx) -> do
    ty1 <- tyOf_ ntx ctx t1
    tyd <- tyOf_ ntx ctx dx
    dxs <- mapM (\(ds, dt) -> (ds,) <$> tyOf_ ntx ctx dt) dxr
    case ty1 of
      TyVariant cx1 cxr ->
        let cxs = cx1 : cxr
         in if all
              ( \(ds, dty) ->
                  any
                    ( \(cs, cty) ->
                        cs == ds && dty == TyArr cty tyd
                    )
                    cxs
              )
              dxs
              then pure tyd
              else error todo
      TyBool -> error todo
      TyInt -> error todo
      TyList _ -> error todo
      _ -> err $ VrMs t1 ty1
  TmCaseOf _ t1 dxr (Left dx) -> do
    ty1 <- tyOf_ ntx ctx t1
    dxty <- tyOf_ ntx ctx (snd dx)
    dxs <- mapM (\(ds, dt) -> (ds,) <$> tyOf_ ntx ctx dt) (dxr <> [dx])
    case dxty of
      TyArr _ tyd ->
        ( case ty1 of
            TyVariant cx1 cxr ->
              let cxs = cx1 : cxr
               in if all
                    ( \(cs, cty) ->
                        any
                          ( \(ds, dty) ->
                              cs == ds && dty == TyArr cty tyd
                          )
                          dxs
                    )
                    cxs
                    && all
                      ( \(ds, dty) ->
                          any
                            ( \(cs, cty) ->
                                cs == ds && dty == TyArr cty tyd
                            )
                            cxs
                      )
                      dxs
                    then pure tyd
                    else error todo
            TyBool -> error todo
            TyInt -> error todo
            TyList _ -> error todo
            _ -> err $ VrMs t1 ty1
        )
      _ -> error todo
  -- list terms.
  TmNil _ ty1 -> pure ty1
  TmCons _ t1 t2 -> do
    (ty1, ty2) <- (,) <$> tyOf_ ntx ctx t1 <*> tyOf_ ntx ctx t2
    case ty2 of
      TyList ty21 | ty1 == ty21 -> pure ty2
      TyList _ -> error todo
      _ -> error todo
  -- base terms.
  TmUnit _ -> pure TyUnit
  TmBool {} -> pure TyBool
  TmInt {} -> pure TyInt
  TmString {} -> pure TyString
  -- syntax sugar.
  TmSeq _ t1 ts -> do
    tys <- mapM (tyOf_ ntx ctx) (t1 : ts)
    pure $ last tys
  TmLet _ x t1 t2 -> do
    ty1 <- tyOf_ ntx ctx t1
    let ctx' = addTy ctx ty1
    let (ntx', _) = newName ntx x
    tyOf_ ntx' ctx' t2
  TmLetRec fi x ty t1 t2 ->
    tyOf_ ntx ctx (TmLet fi x (TmFix fi (TmAbs fi (Just x) ty t1)) t2)
  TmIf _ t1 t2 t3 -> do
    (ty1, ty2, ty3) <-
      (,,)
        <$> tyOf_ ntx ctx t1
        <*> tyOf_ ntx ctx t2
        <*> tyOf_ ntx ctx t3
    if ty1 == TyBool
      then
        if ty2 == ty3
          then pure ty2
          else err $ CaseMs t1 ty1 t2 ty2
      else err $ IfMs t1 ty1
  TmAdd _ t1 t2 -> bin TyInt ntx ctx t1 t2
  TmSub _ t1 t2 -> bin TyInt ntx ctx t1 t2
  TmMul _ t1 t2 -> bin TyInt ntx ctx t1 t2
  TmDiv _ t1 t2 -> bin TyInt ntx ctx t1 t2
  TmRem _ t1 t2 -> bin TyInt ntx ctx t1 t2
  _ -> error $ "TODO: type checking is not implemented for " <> prTm ntx tm

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

bin :: Ty -> NameCtx -> TyCtx -> Tm -> Tm -> Fallible TyErr Ty
bin ty ntx ctx t1 t2 = do
  (ty1, ty2) <- (,) <$> tyOf_ ntx ctx t1 <*> tyOf_ ntx ctx t2
  if ty1 == ty
    then
      if ty2 == ty
        then pure ty
        else err $ AppMs t2 ty2 ty
    else err $ AppMs t1 ty1 ty

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

testTypechecker :: IO ()
testTypechecker = runTestTT tests $> ()

-------------------------------------------------------------------------------

tests :: Test
tests =
  test
    [ "o-base-1" ~: Okay TyUnit ~=? tyOf (TmUnit TNA),
      "o-base-2" ~: Okay TyBool ~=? tyOf (TmBool TNA True),
      "o-base-3" ~: Okay TyInt ~=? tyOf (TmInt TNA 0),
      "o-base-4" ~: Okay TyString ~=? tyOf (TmString TNA "apples!"),
      "o-core-1"
        ~: Okay (TyArr TyInt TyUnit)
        ~=? tyOf (TmAbs TNA Nothing TyInt (TmUnit TNA)),
      "o-core-2"
        ~: Okay (TyArr TyInt TyInt)
        ~=? tyOf (TmAbs TNA Nothing TyInt (TmVar TNA 0 1)),
      "o-core-3"
        ~: Okay TyInt
        ~=? tyOf
          ( TmApp
              TNA
              (TmAbs TNA Nothing TyInt (TmVar TNA 0 1))
              (TmInt TNA 5)
          ),
      "o-core-4"
        ~: Okay (TyArr TyBool TyBool)
        ~=? tyOf
          ( TmApp
              TNA
              (TmAbs TNA Nothing (TyArr TyBool TyBool) (TmVar TNA 0 1))
              (TmAbs TNA Nothing TyBool (TmVar TNA 0 1))
          ),
      "f-core-1" ~: Failed [FreeVar 0] ~=? tyOf (TmVar TNA 0 0),
      "f-core-2"
        ~: Failed [FreeVar 0]
        ~=? tyOf (TmAbs TNA (Just "x") TyUnit (TmVar TNA 1 1)),
      "f-core-3"
        ~: Failed [AppMs (TmInt TNA 5) TyInt TyBool]
        ~=? tyOf
          ( TmApp
              TNA
              ( TmAbs
                  TNA
                  (Just "x")
                  TyBool
                  (TmVar TNA 0 1)
              )
              (TmInt TNA 5)
          ),
      "f-core-4"
        ~: Failed [ArrMs (TmUnit TNA) TyUnit (TmBool TNA True) TyBool]
        ~=? tyOf
          ( TmApp
              TNA
              (TmUnit TNA)
              (TmBool TNA True)
          ),
      "f-core-5" -- is this the desired behavior?
        ~: Failed [FreeVar 0]
        ~=? tyOf
          ( TmApp
              TNA
              ( TmAbs
                  TNA
                  (Just "x")
                  (TyArr TyString TyBool)
                  (TmVar TNA 1 1)
              )
              (TmUnit TNA)
          ),
      "f-core-6"
        ~: Failed [FreeVar 0, FreeVar 1, FreeVar 2]
        ~=? tyOf
          ( TmApp
              TNA
              (TmVar TNA 0 0)
              ( TmApp TNA (TmVar TNA 1 0) (TmVar TNA 2 0)
              )
          ),
      "f-core-7" -- is this the desired behavior?
        ~: Failed [AppMs (TmUnit TNA) TyUnit TyBool]
        ~=? tyOf
          ( TmApp
              TNA
              ( TmAbs
                  TNA
                  (Just "x")
                  TyString
                  (TmVar TNA 0 1)
              )
              ( TmApp
                  TNA
                  ( TmAbs
                      TNA
                      (Just "x")
                      TyBool
                      (TmInt TNA 5)
                  )
                  (TmUnit TNA)
              )
          ),
      "o-control-1"
        ~: Okay TyUnit
        ~=? tyOf
          ( TmFix
              TNA
              (TmAbs TNA (Just "x") TyUnit (TmVar TNA 0 1))
          ),
      "o-control-2"
        ~: Okay TyBool
        ~=? tyOf
          ( TmBEq
              TNA
              (TmInt TNA 5)
              (TmInt TNA (-7))
          ),
      "o-control-3"
        ~: Okay TyUnit
        ~=? tyOf
          ( TmPrint
              TNA
              (TmString TNA "apples!")
          ),
      "o-control-4" -- is this right?
        ~: Okay TyString
        ~=? tyOf
          ( TmApp
              TNA
              ( TmAbs
                  TNA
                  (Just "x")
                  TyBool
                  (TmString TNA "hello world!")
              )
              ( TmBEq
                  TNA
                  ( TmFix -- never terminates, but is still of type int.
                      TNA
                      ( TmAbs -- divergent function.
                          TNA
                          (Just "x")
                          TyInt
                          (TmVar TNA 0 1)
                      )
                  )
                  (TmInt TNA 2)
              )
          ),
      -- "f-control-1" ~: False ~=? True,
      "o-product-1"
        ~: Okay (TyTuple TyUnit [])
        ~=? tyOf (TmTuple TNA (TmUnit TNA) []),
      "o-product-2"
        ~: Okay (TyTuple TyInt [TyArr TyString TyUnit, TyBool])
        ~=? tyOf
          ( TmTuple
              TNA
              (TmInt TNA 3)
              [ TmAbs TNA (Just "x") TyString (TmPrint TNA (TmVar TNA 0 1)),
                TmBool TNA False
              ]
          ),
      "o-product-3"
        ~: Okay (TyArr TyString TyUnit)
        ~=? tyOf
          ( TmProj
              TNA
              ( TmTuple
                  TNA
                  (TmInt TNA 3)
                  [ TmAbs TNA (Just "x") TyString (TmPrint TNA (TmVar TNA 0 1)),
                    TmBool TNA False
                  ]
              )
              1
          ),
      "o-product-4"
        ~: Okay TyInt
        ~=? tyOf
          ( TmProj
              TNA
              ( TmTuple
                  TNA
                  (TmInt TNA 3)
                  [ TmAbs TNA (Just "x") TyString (TmPrint TNA (TmVar TNA 0 1)),
                    TmBool TNA False
                  ]
              )
              0
          ),
      "o-product-5"
        ~: Okay TyBool
        ~=? tyOf
          ( TmProj
              TNA
              ( TmTuple
                  TNA
                  (TmInt TNA 3)
                  [ TmAbs TNA (Just "x") TyString (TmPrint TNA (TmVar TNA 0 1)),
                    TmBool TNA False
                  ]
              )
              2
          ),
      -- "f-product-1" ~: False ~=? True,
      "o-sum-1"
        ~: Okay (TyVariant ("Some", TyInt) [("None", TyUnit)])
        ~=? tyOf
          ( TmVariant
              TNA
              "Some"
              (TmInt TNA 5)
              (TyVariant ("Some", TyInt) [("None", TyUnit)])
          ),
      "o-sum-2"
        ~: Okay (TyVariant ("Some", TyInt) [("None", TyUnit)])
        ~=? tyOf
          ( TmVariant
              TNA
              "None"
              (TmUnit TNA)
              (TyVariant ("Some", TyInt) [("None", TyUnit)])
          ),
      "o-sum-3"
        ~: Okay (TyVariant ("Some", TyInt) [("None", TyUnit), ("Maybe", TyArr TyUnit TyInt)])
        ~=? tyOf
          ( TmVariant
              TNA
              "Maybe"
              (TmAbs TNA (Just "x") TyUnit (TmInt TNA 5))
              (TyVariant ("Some", TyInt) [("None", TyUnit), ("Maybe", TyArr TyUnit TyInt)])
          ),
      "o-sum-4"
        ~: Okay TyBool
        ~=? tyOf
          ( TmCaseOf
              TNA
              ( TmVariant
                  TNA
                  "Maybe"
                  (TmAbs TNA (Just "x") TyUnit (TmInt TNA 5))
                  (TyVariant ("Some", TyInt) [("None", TyUnit), ("Maybe", TyArr TyUnit TyInt)])
              )
              [ ("Some", TmAbs TNA (Just "x") TyInt (TmBool TNA True)),
                ("None", TmAbs TNA (Just "x") TyUnit (TmBool TNA False))
              ]
              (Left ("Maybe", TmAbs TNA (Just "y") (TyArr TyUnit TyInt) (TmBool TNA True)))
          ),
      "o-sum-5"
        ~: Okay TyBool
        ~=? tyOf
          ( TmCaseOf
              TNA
              ( TmVariant
                  TNA
                  "Maybe"
                  (TmAbs TNA (Just "x") TyUnit (TmInt TNA 5))
                  (TyVariant ("Some", TyInt) [("None", TyUnit), ("Maybe", TyArr TyUnit TyInt)])
              )
              [ ("Some", TmAbs TNA (Just "x") TyInt (TmBool TNA True)),
                ("None", TmAbs TNA (Just "x") TyUnit (TmBool TNA False)),
                ("Maybe", TmAbs TNA (Just "y") (TyArr TyUnit TyInt) (TmBool TNA True))
              ]
              (Right (TmBool TNA False))
          ),
      "o-sum-6"
        ~: Okay TyBool
        ~=? tyOf
          ( TmCaseOf
              TNA
              ( TmVariant
                  TNA
                  "Maybe"
                  (TmAbs TNA (Just "x") TyUnit (TmInt TNA 5))
                  (TyVariant ("Some", TyInt) [("None", TyUnit), ("Maybe", TyArr TyUnit TyInt)])
              )
              [("Some", TmAbs TNA (Just "x") TyInt (TmBool TNA True))]
              (Right (TmBool TNA False))
          ),
      -- "f-sum-1" ~: False ~=? True,
      -- "o-list-1" ~: False ~=? True,
      -- "f-list-1" ~: False ~=? True,
      -- "o-sugar-1" ~: False ~=? True,
      -- "f-sugar-1" ~: False ~=? True,
      "o-complex-1-factorial"
        ~: Okay TyInt
        ~=? tyOf (tmFac 5),
      "o-complex-1-factorial-rec"
        ~: Okay TyInt
        ~=? tyOf (tmFacR 5),
      -- "f-complex-1" ~: False ~=? True,
      "prop-base-safe" ~: ptest prop_base_safe
    ]

-- TODO: properties for each typing rule.
-- TODO: properties for evaluation safety (only for total expressions).
-- 1) if (tm --> stuck) then (tyOf tm == Failed {})
-- 2) if (tyOf tm == Okay ty) then (tm --> val ... of ty)

-------------------------------------------------------------------------------

prop_base_safe :: Tm -> Property
prop_base_safe t1 =
  tmBase t1 ==>
    let ty = tyOf t1
     in ty == Okay TyUnit
          .||. ty == Okay TyBool
          .||. ty == Okay TyInt
          .||. ty == Okay TyString

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
