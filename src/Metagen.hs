module Metagen
    ( main
    ) where


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


-- for test only
main = do
  let
      c :: [nat|1|]
      c = [nat|1|]

      hlist = [hl|1, 2, 3, 5|]

      hlist' :: [hl|Int, Int, Int, Int|]
      hlist' = hlist
  print $ hlist' ! [nat|3|]
  putStrLn "FLam"
