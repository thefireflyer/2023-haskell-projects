module SLang.Interpreter (testInterpreter, eval) where

import Common
import Data.Bifunctor (Bifunctor (second))
import Data.Functor (($>))
import Data.List (find)
import SLang.DeBruijn
import Test.HUnit

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

eval1 :: NameCtx -> Tm -> IO Tm
eval1 ctx tm =
  -- putStrLn ("\nevaluating " <> prTm ctx tm) *>
  case tm of
    -- core rules.
    TmApp _ (TmAbs _ _ _ t12) v2
      | tmVal v2 ->
          pure $ tmSubstTop v2 t12
    TmApp fi v1 t2
      | tmVal v1 ->
          TmApp fi v1 <$> eval1 ctx t2
    TmApp fi t1 t2 ->
      flip (TmApp fi) t2 <$> eval1 ctx t1
    -- fix rules.
    TmFix _ (TmAbs _ _ _ t2) ->
      pure $ tmSubstTop tm t2
    TmFix fi t2 ->
      TmFix fi <$> eval1 ctx t2
    -- boolean equality rules.
    TmBEq fi v1 v2
      | tmVal v1 && tmVal v2 ->
          pure $ TmBool fi (v1 == v2)
    TmBEq fi v1 t2
      | tmVal v1 ->
          TmBEq fi v1 <$> eval1 ctx t2
    TmBEq fi t1 t2 ->
      flip (TmBEq fi) t2 <$> eval1 ctx t1
    -- print rules.
    TmPrint fi (TmString _ s) ->
      putStr s $> TmUnit fi
    TmPrint fi t1 ->
      TmPrint fi <$> eval1 ctx t1
    -- show rules.
    TmShow fi v1
      | tmVal v1 ->
          pure $ TmString fi (prTm [] v1)
    TmShow fi t1 ->
      TmShow fi <$> eval1 ctx t1
    -- read rules.
    TmRead fi v1 ty1
      | tmVal v1 ->
          error todo
    TmRead fi t1 ty1 ->
      flip (TmRead fi) ty1 <$> eval1 ctx t1
    -- error rules.
    TmError fi s _ ->
      ioError . userError $
        "Encountered runtime error:\n"
          <> prTI fi
          <> "\t"
          <> s
    -- tuple rules.
    TmTuple fi v1 ts
      | tmVal v1 && not (all tmVal ts) -> do
          (vs, t2 : tr) <- pure $ span tmVal ts
          v2 <- eval1 ctx t2
          pure $ TmTuple fi v1 (vs <> (v2 : tr))
    TmTuple fi t1 ts ->
      flip (TmTuple fi) ts <$> eval1 ctx t1
    -- projection rules.
    TmProj _ (TmTuple _ v1 vs) i
      | tmVal v1 && all tmVal vs ->
          if i == 0 then pure v1 else pure $ vs !! (i - 1)
    TmProj fi t1 i ->
      flip (TmProj fi) i <$> eval1 ctx t1
    -- variant rules.
    TmVariant fi s t1 ty ->
      flip (TmVariant fi s) ty <$> eval1 ctx t1
    -- case of rules (variant).
    TmCaseOf _ (TmVariant _ s v1 _) dxr dx
      | tmVal v1 -> pure $ handleCaseOf s [v1] dxr dx
    -- case of rules (list).
    TmCaseOf _ v1@(TmNil {}) dxr dx ->
      pure $ handleCaseOf "Nil" [v1] dxr dx
    TmCaseOf _ (TmCons _ v1 v2) dxr dx
      | tmVal v1 && tmVal v2 ->
          pure $ handleCaseOf "Cons" [v1, v2] dxr dx
    -- case of rules (bool).
    TmCaseOf _ (TmBool _ b) dxr dx ->
      pure $ handleCaseOf (show b) [TmUnit TNA] dxr dx
    -- case of rules (int).
    TmCaseOf _ (TmInt _ 0) dxr dx ->
      pure $ handleCaseOf "Zero" [TmUnit TNA] dxr dx
    TmCaseOf _ (TmInt _ n) dxr dx ->
      pure $ handleCaseOf "Succ" [TmInt TNA (n - 1)] dxr dx
    -- case of rules.
    TmCaseOf fi t1 dxr dx ->
      flip (flip (TmCaseOf fi) dxr) dx <$> eval1 ctx t1
    -- cons rules.
    TmCons fi v1 t2
      | tmVal v1 ->
          TmCons fi v1 <$> eval1 ctx t2
    TmCons fi t1 t2 ->
      flip (TmCons fi) t2 <$> eval1 ctx t1
    -- seq rules.
    TmSeq _ v1 []
      | tmVal v1 -> pure v1
    TmSeq fi v1 (v2 : ts)
      | tmVal v1 && tmVal v2 ->
          pure $ TmSeq fi v2 ts
    TmSeq fi v1 (t2 : ts)
      | tmVal v1 ->
          flip (TmSeq fi) ts <$> eval1 ctx t2
    TmSeq fi t1 ts ->
      flip (TmSeq fi) ts <$> eval1 ctx t1
    -- let rules.
    TmLet _ _ v1 t2
      | tmVal v1 ->
          pure $ tmSubstTop v1 t2
    TmLet fi x t1 t2 ->
      flip (TmLet fi x) t2 <$> eval1 ctx t1
    -- let rec rules.
    TmLetRec fi x ty t1 t2 ->
      pure $ TmLet fi x (TmFix fi (TmAbs fi (Just x) ty t1)) t2
    -- isnil rules.
    TmIsNil fi (TmNil {}) ->
      pure $ TmBool fi True
    TmIsNil fi (TmCons _ v1 v2)
      | tmVal v1 && tmVal v2 ->
          pure $ TmBool fi True
    TmIsNil fi t1 ->
      TmIsNil fi <$> eval1 ctx t1
    -- head rules.
    TmHead fi (TmNil _ ty) ->
      pure $
        TmVariant
          fi
          "None"
          (TmUnit TNA)
          (TyVariant ("Some", TyList ty) [("None", TyUnit)])
    TmHead fi (TmCons _ v1 v2)
      | tmVal v1 && tmVal v2 ->
          pure $
            TmVariant
              fi
              "Some"
              v1
              (TyVariant ("Some", TyList (getListTy v2)) [("None", TyUnit)])
    TmHead fi t1 ->
      TmHead fi <$> eval1 ctx t1
    -- tail rules.
    TmTail _ v1@(TmNil {}) -> pure v1
    TmTail _ (TmCons _ v1 v2)
      | tmVal v1 && tmVal v2 ->
          pure v2
    TmTail fi t1 ->
      TmTail fi <$> eval1 ctx t1
    -- if rules.
    TmIf _ (TmBool _ b) t2 t3 ->
      pure $ if b then t2 else t3
    TmIf fi t1 t2 t3 ->
      flip (flip (TmIf fi) t2) t3 <$> eval1 ctx t1
    -- add rules.
    TmAdd fi (TmInt _ v1) (TmInt _ v2) ->
      pure $ TmInt fi (v1 + v2)
    TmAdd fi v1 t2
      | tmVal v1 ->
          TmAdd fi v1 <$> eval1 ctx t2
    TmAdd fi t1 t2 ->
      flip (TmAdd fi) t2 <$> eval1 ctx t1
    -- sub rules.
    TmSub fi (TmInt _ v1) (TmInt _ v2) ->
      pure $ TmInt fi (v1 - v2)
    TmSub fi v1 t2
      | tmVal v1 ->
          TmSub fi v1 <$> eval1 ctx t2
    TmSub fi t1 t2 ->
      flip (TmSub fi) t2 <$> eval1 ctx t1
    -- mul rules.
    TmMul fi (TmInt _ v1) (TmInt _ v2) ->
      pure $ TmInt fi (v1 * v2)
    TmMul fi v1 t2
      | tmVal v1 ->
          TmMul fi v1 <$> eval1 ctx t2
    TmMul fi t1 t2 ->
      flip (TmMul fi) t2 <$> eval1 ctx t1
    -- div rules.
    TmDiv fi (TmInt _ v1) (TmInt _ v2) ->
      pure $ TmInt fi (quot v1 v2)
    TmDiv fi v1 t2
      | tmVal v1 ->
          TmDiv fi v1 <$> eval1 ctx t2
    TmDiv fi t1 t2 ->
      flip (TmDiv fi) t2 <$> eval1 ctx t1
    -- rem rules.
    TmRem fi (TmInt _ v1) (TmInt _ v2) ->
      pure $ TmInt fi (rem v1 v2)
    TmRem fi v1 t2
      | tmVal v1 ->
          TmRem fi v1 <$> eval1 ctx t2
    TmRem fi t1 t2 ->
      flip (TmRem fi) t2 <$> eval1 ctx t1
    -- failure case.
    _ -> error $ "No rule applies to " <> prTm ctx tm

eval :: NameCtx -> Tm -> IO Tm
eval _ t | tmVal t = pure t
eval ctx t = eval1 ctx t >>= eval ctx

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

handleCaseOf :: String -> [Tm] -> [(String, Tm)] -> Either (String, Tm) Tm -> Tm
handleCaseOf s args dxr (Left dx) =
  case find ((==) s . fst) (dx : dxr) of
    Just (_, t1) -> foldl wrapApp t1 args
    _ -> error "???"
handleCaseOf s args dxr (Right dx) =
  case find ((==) s . fst) dxr of
    Just (_, t1) -> foldl wrapApp t1 args
    _ -> dx

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

tmMap :: (Int -> TI -> Int -> Int -> Tm) -> Tm -> Tm
tmMap onvar t =
  let walk c tm = case tm of
        -- core.
        TmVar fi x n -> onvar c fi x n
        TmAbs fi x ty t1 -> TmAbs fi x ty (walk (c + 1) t1)
        TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)
        -- control flow.
        TmFix fi t1 -> TmFix fi (walk c t1)
        TmBEq fi t1 t2 -> TmBEq fi (walk c t1) (walk c t2)
        TmPrint fi t1 -> TmPrint fi (walk c t1)
        TmShow fi t1 -> TmShow fi (walk c t1)
        TmRead fi t1 ty1 -> TmRead fi (walk c t1) ty1
        TmError {} -> tm
        -- products.
        TmTuple fi t1 ts -> TmTuple fi (walk c t1) (map (walk c) ts)
        TmProj fi t1 i -> TmProj fi (walk c t1) i
        -- sums.
        TmVariant fi s t1 ty -> TmVariant fi s (walk c t1) ty
        TmCaseOf fi t1 dxr (Left dx) ->
          TmCaseOf
            fi
            (walk c t1)
            (map (second (walk c)) dxr)
            (Left (second (walk c) dx))
        TmCaseOf fi t1 dxr (Right w) ->
          TmCaseOf
            fi
            (walk c t1)
            (map (second (walk c)) dxr)
            (Right (walk c w))
        -- lists.
        TmNil {} -> tm
        TmCons fi t1 t2 -> TmCons fi (walk c t1) (walk c t2)
        -- bases.
        TmUnit {} -> tm
        TmBool {} -> tm
        TmInt {} -> tm
        TmString {} -> tm
        -- sugar.
        TmSeq fi t1 ts -> TmSeq fi (walk c t1) (map (walk c) ts)
        TmLet fi x t1 t2 -> TmLet fi x (walk c t1) (walk (c + 1) t2)
        TmLetRec fi x ty t1 t2 ->
          TmLetRec fi x ty (walk (c + 1) t1) (walk (c + 1) t2)
        TmIsNil fi t1 -> TmIsNil fi (walk c t1)
        TmHead fi t1 -> TmHead fi (walk c t1)
        TmTail fi t1 -> TmTail fi (walk c t1)
        TmIf fi t1 t2 t3 -> TmIf fi (walk c t1) (walk c t2) (walk c t3)
        TmAdd fi t1 t2 -> TmAdd fi (walk c t1) (walk c t2)
        TmSub fi t1 t2 -> TmSub fi (walk c t1) (walk c t2)
        TmMul fi t1 t2 -> TmMul fi (walk c t1) (walk c t2)
        TmDiv fi t1 t2 -> TmDiv fi (walk c t1) (walk c t2)
        TmRem fi t1 t2 -> TmRem fi (walk c t1) (walk c t2)
   in walk 0 t

tmShift :: Int -> Tm -> Tm
tmShift d t =
  let onvar c fi x n =
        if x >= c
          then TmVar fi (x + d) (n + d)
          else TmVar fi x (n + d)
   in tmMap onvar t

tmSubst :: Int -> Tm -> Tm -> Tm
tmSubst j s t =
  let onvar c fi x n =
        if x == j + c then tmShift c s else TmVar fi x n
   in tmMap onvar t

tmSubstTop :: Tm -> Tm -> Tm
tmSubstTop s t = tmShift (-1) $ tmSubst 0 (tmShift 1 s) t

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------

testInterpreter :: IO ()
testInterpreter = runTestTT tests $> ()

(>=<) :: Tm -> Tm -> IO ()
(>=<) t1 t0 = eval [] t0 >>= assertEqual (prTm [] t0) t1

tests :: Test
tests =
  test
    [ "o-base-1" ~: TmUnit TNA >=< TmUnit TNA,
      "o-base-2" ~: TmBool TNA True >=< TmBool TNA True,
      "o-base-3" ~: TmInt TNA 5 >=< TmInt TNA 5,
      "o-base-4" ~: TmString TNA "oranges" >=< TmString TNA "oranges",
      "o-core-1"
        ~: TmAbs TNA (Just "x") TyUnit (TmVar TNA 0 1)
        >=< TmAbs TNA (Just "x") TyUnit (TmVar TNA 0 1),
      "o-core-2"
        ~: TmUnit TNA
        >=< TmApp
          TNA
          (TmAbs TNA (Just "x") TyUnit (TmVar TNA 0 1))
          (TmUnit TNA),
      "o-complex-1-factorial-1"
        ~: TmInt TNA 1
        >=< tmFac 0,
      "o-complex-1-factorial-2"
        ~: TmInt TNA 1
        >=< tmFac 1,
      "o-complex-1-factorial-3"
        ~: TmInt TNA 120
        >=< tmFac 5,
      "o-complex-1-factorial-4"
        ~: TmInt TNA 2432902008176640000
        >=< tmFac 20,
      "o-complex-1-factorial-rec-1"
        ~: TmInt TNA 1
        >=< tmFacR 0,
      "o-complex-1-factorial-rec-2"
        ~: TmInt TNA 1
        >=< tmFacR 1,
      "o-complex-1-factorial-rec-3"
        ~: TmInt TNA 120
        >=< tmFacR 5,
      "o-complex-1-factorial-rec-4"
        ~: TmInt TNA 2432902008176640000
        >=< tmFacR 20,
      "test2" ~: True ~=? True
    ]

-------------------------------------------------------------------------------
--- /////////////////////////////////////////////////////////////////////// ---
-------------------------------------------------------------------------------
