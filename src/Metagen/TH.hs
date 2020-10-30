module Metagen.TH
  ( gVisitExp, gVisitType, Self(..), mkSelf, mkSimpleQQ
  , parseExpAndMap, parseTypeAndMap
  )
where

import qualified Metagen.MetaParsePatch  as MetaParse
import           Language.Haskell.TH         as TH
import           Language.Haskell.TH.Quote   as TH
import           Language.Haskell.TH.Syntax  as TH

data Self
  = Self { vExp :: Exp -> Exp, vType :: Type -> Type }

gVisitType Self {vType} = \case
  ForallT tvars ctx t -> ForallT tvars ctx $ vType t
  AppT t1 t2 -> AppT (vType t1) (vType t2)
  AppKindT t1 k -> AppKindT (vType t1) k
  SigT t k -> SigT (vType t) k
  InfixT t1 n t2 -> InfixT (vType t1) n (vType t2)
  UInfixT t1 n t2 -> UInfixT (vType t1) n (vType t2)
  ParensT t -> ParensT $ vType t
  ImplicitParamT s t ->  ImplicitParamT s $ vType t
  a -> a


gVisitExp Self {vExp=v, vType} = \case
   AppE f a -> AppE (v f) (v a)
   AppTypeE f a -> AppTypeE (v f) (vType a)
   InfixE l e r -> InfixE (v `fmap` l) (v e) (v `fmap` r)
   UInfixE e1 e2 e3 -> UInfixE (v e1) (v e2) (v e3)
   ParensE e -> ParensE $ v e
   LamE p e -> LamE p $ v e  -- p can contain Expr
   LamCaseE _ -> error "not implemented"

   TupE xs -> TupE (v `fmap` xs)
   CondE cond e1 e2 -> CondE (v cond) (v e1) (v e2)
   ListE xs -> ListE $ v `fmap` xs
   SigE e t -> SigE (v e) (vType t)
   StaticE e -> StaticE $ v e

   RecUpdE _ _ -> error "not implemented"
   RecConE _ _ -> error "not implemented"
   UnboxedTupE _ -> error "not implemented"
   UnboxedSumE _ _ _ -> error "not implemented"
   MultiIfE _ -> error "not implemented"

   LetE _ _ -> error "not implemented"
   CaseE _ _ -> error "not implemented"
   DoE _ -> error "not implemented"
   MDoE _ -> error "not implemented"
   CompE _ -> error "not implemented"
   ArithSeqE _ -> error "not implemented"
   a -> a


mkSelf vExp vType = self
  where
    self = Self {vExp=vExp self, vType=vType self}

parseExpAndMap f s = case MetaParse.parseExp s of
   Right a -> return (f a)
   Left s  -> error s

parseTypeAndMap f s = case MetaParse.parseType s of
  Right a -> return (f a)
  Left s  -> error s


mkSimpleQQ quoteExp quoteType = TH.QuasiQuoter {quoteExp, quoteType, quotePat, quoteDec}
  where
      notHandled  things = error $ things ++ " are not handled by the regex quasiquoter."
      quoteDec = notHandled "declarations"
      quotePat = notHandled "patterns"
