-- implementing 2 contextual quasi-quotation
-- 'nat': building expressions or types for dependent natural numbers
-- 'hl': building  expressions or types for heterogeneous lists
module Metagen.Templates
    (nat, hl)
where

import           Language.Haskell.TH         as TH
import           Language.Haskell.TH.Quote   as TH
import           Language.Haskell.TH.Syntax  as TH
import           Metagen.TH

nat :: QuasiQuoter
nat = mkSimpleQQ (parseExpAndMap exp') (parseTypeAndMap typ')
  where
    zeroName = mkName "Z"
    succName = mkName "S"

    mkNatE n
      | n < 0 = error "invalid natural number"
      | n == 0 = ConE zeroName
      | otherwise = AppE (ConE succName) $ mkNatE (n-1)

    mkNatT n
      | n < 0 = error "invalid natural number"
      | n == 0 = ConT zeroName
      | otherwise = AppT (ConT succName) $ mkNatT (n-1)

    self = Self exp' typ'
    exp' = \case
      a@ (LitE (IntegerL i)) -> mkNatE i
      a -> gVisitExp self a

    typ' = \case
      a@ (LitT (NumTyLit i)) -> mkNatT i
      a -> gVisitType self a

wrap_bracket x = "[" ++ x ++ "]"
hl :: QuasiQuoter
hl = mkSimpleQQ (parseExpAndMap exp' . wrap_bracket) (parseTypeAndMap typ' . wrap_bracket)
  where
    nilName = mkName "HNil"
    consName = mkName "HCons"

    
    self = Self exp' typ'

    mkHListExp = \case
      [] -> ConE nilName
      hd:tl -> AppE (AppE (ConE consName) (vExp self hd)) (mkHListExp tl)

    exp' = \case
      ListE xs -> mkHListExp xs
      a -> gVisitExp self a

    typ' = \case
      AppT (AppT PromotedConsT hd) tl ->
        AppT (AppT (ConT consName) (vType self hd)) (vType self tl)
      PromotedNilT -> ConT nilName
      a -> gVisitType self a
  