module Metagen.Python
where

import           Control.Monad.State.Lazy
import           Data.List                (intercalate)
import           Metagen.Infr
import           Metagen.Templates
import           Text.Printf              (printf)

data PList a
data PSet a
data PDict k v
data PTup l

class ToPyCode a where
  toPyCode :: [(String, Bool)] ->  a -> String

instance ToPyCode HNil where
  toPyCode _ _ = ","

instance (HList tl, ToPyCode tl) =>
         ToPyCode (HCons (PyExp hd) tl) where
  toPyCode ns (HCons hd tl) = toPyCode ns hd ++ ", " ++ toPyCode ns tl


data PyExp a where
  PyCall :: PyExp (a -> b) -> PyExp a -> PyExp b
  PyInt  :: Int -> PyExp Int
  PyStr  :: String -> PyExp String
  PyNum  :: Double -> PyExp Double
  PyList :: [PyExp a] -> PyExp (PList a)
  PySet  :: [PyExp a] -> PyExp (PSet a)
  PyDict :: [(PyExp k, PyExp v)] -> PyExp (PDict k v)
  PyTup  :: (ToPyCode l, HList l) => l -> PyExp (PTup l)
  GS :: String -> PyExp a
  LS :: Int -> PyExp a

instance ToPyCode (PyExp a) where
  toPyCode ns = \case
      PyCall f a -> printf "%s(%s)" (toPyCode ns f) (toPyCode ns a)
      PyInt i -> printf "%d" i
      PyNum f -> printf "%f" f
      PyStr s -> show s
      PyList xs -> printf "[%s]" $ intercalate ", " $ toPyCode ns `map` xs
      PySet [] -> "set()"
      PySet xs -> printf "{%s}" $ intercalate ", " $ toPyCode ns `map` xs
      PyDict xs -> printf "{%s}" $ intercalate ", " $
        xs >>= \(k, v) -> [toPyCode ns k, ": ", toPyCode ns v]
      PyTup l -> printf "(%s)%" $ toPyCode ns l
      GS n -> n
      LS n -> fst (ns !! n)

data PyState
  = PyState
  { code   :: [(String, Bool)] -> [String]
  , vregs  :: [(String, Bool)] -- [(name, isUsed)]
  , layout :: String
  }

emptyPyState = PyState {code = const [], vregs = [], layout = []}

type PyMState a = State PyState a

allocate_id :: String -> PyMState Int
allocate_id s = state $ \st ->
  let n = length $ vregs st in
  (n, st { vregs = (s, False) : vregs st })

mksymbol :: String -> PyMState (PyExp a)
mksymbol s = do
  n <- allocate_id s
  return (LS n)

use :: PyExp a -> PyMState (PyExp a)
use a = do
  st <- get
  case a of
    LS n ->
      do
        let vregs' = updateL (vregs st) n (\(name, _) -> (name, True))
        put $ st { vregs = vregs' }
        return a
    PyCall _ _ ->
      do
        n <- allocate_id "call"      
        let PyState {layout, code} = st
        let pysym = LS n
        let code' :: [(String, Bool)] -> [String]
            code' defuse
              | snd (defuse !! n) =
                  printf "%s%s" layout (toPyCode defuse pysym) (toPyCode defuse a):tl
              | otherwise = printf "%s%s" layout (toPyCode defuse a):tl
              where tl = code defuse
        put $ st {code = code'}
        return pysym
    a -> return a

class MkC a where
  mkc :: a -> PyExp a

instance MkC Int where
  mkc = PyInt

instance MkC Double where
  mkc = PyNum

instance MkC String where
  mkc = PyStr

updateL :: [a] -> Int -> (a -> a) -> [a]
updateL (x:xs) 0 f = f x : xs
updateL (x:xs) n f = x : updateL xs n f
updateL _ _ _ = error "invalid index"

indentInc :: Int -> PyMState ()
indentInc n = do
  st <- get
  if  | n == 0 -> return ()
      | n < 0  -> put $ st {layout = drop n $ layout st}
      | n > 0  -> put $ st {layout = replicate n ' ' ++ layout st}

INDENT = indentInc 2
DEDENT = indentInc -2

assign :: PyExp a -> PyExp a -> PyMState ()
assign tag value = case tag of
  LS _ ->
      
  
