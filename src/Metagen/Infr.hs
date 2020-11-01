module Metagen.Infr
 ( HList(..), HCons(..), HNil(..)
 , Nat(..), S(..), Z(..), Indexable(..)
 )
where

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Metagen.Templates

-- http://okmij.org/ftp/Haskell/HList-ext.pdf
class HList l

data HNil = HNil deriving (Eq, Show, Read)
data HCons e a = HCons e a deriving (Eq, Show, Read)

instance HList HNil
instance HList tl => HList (HCons e tl)

class Nat a
data S a = S a deriving (Show, Eq, Read)
data Z = Z deriving (Show, Eq, Read)

instance Nat Z
instance Nat a => Nat (S a)

class Indexable l i e | l i -> e where
  (!) :: l -> i -> e

instance Indexable (HCons hd tl) Z hd where
  (HCons hd _) ! _ = hd

instance (Nat n, HList l, Indexable l n e) =>
          Indexable (HCons hd l) (S n) e where
  (HCons _ tl) ! (S a) = tl ! a

data SList n e where
  SNil  :: SList Z a
  SCons :: a -> SList n a -> SList (S n) a

instance Eq (SList Z a) where
  _==_ = True

deriving instance (Eq a, Eq (SList n a)) => Eq (SList (S n) a)

instance Ord (SList Z a) where
  _ `compare` _ = EQ

deriving instance (Ord a, Ord (SList n a)) => Ord (SList (S n) a)

instance Functor (SList Z) where
  f `fmap` SNil = SNil

instance Functor (SList n) => Functor (SList (S n)) where
  fmap f (SCons hd tl) = SCons (f hd) $ fmap f tl




