module Metagen.Python where

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
  PyBool :: Bool -> PyExp Bool
  PyNum  :: Double -> PyExp Double
  PyList :: [PyExp a] -> PyExp (PList a)
  PySet  :: [PyExp a] -> PyExp (PSet a)
  PyDict :: [(PyExp k, PyExp v)] -> PyExp (PDict k v)
  PyTup  :: (ToPyCode l, HList l) => l -> PyExp (PTup l)
  GS :: String -> PyExp a
  LS :: Int -> PyExp a

  PyIn :: PContainer c e => PyExp e -> PyExp c -> PyExp Bool
  PyIndex :: PSequence c i e => PyExp c -> PyExp i -> PyExp e
  PyAnd :: PyExp Bool -> PyExp Bool -> PyExp Bool
  PyOr :: PyExp Bool -> PyExp Bool -> PyExp Bool

instance ToPyCode (PyExp a) where
  toPyCode ns = \case
      PyOr a b -> printf "(%s or %s)" (toPyCode ns a) (toPyCode ns b)
      PyAnd a b -> printf "(%s and %s)" (toPyCode ns a) (toPyCode ns b)
      PyBool True -> "True"
      PyBool False -> "False"
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
      LS n -> printf "_lc_%d_%s" (n) (fst (ns !! n))

type DefUse = [(String, Bool)]
data PyState
  = PyState
  { code   :: DefUse -> [String]
  , vregs  :: DefUse -- [(name, isUsed)]
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

isUsed :: Int -> DefUse -> Bool
isUsed n defuse = snd (defuse !! n)

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
        let pysym = LS n
        appendCode $ \defuse ->
          if | isUsed n defuse ->
               let target = toPyCode defuse pysym
                   value  = toPyCode defuse a
               in  [printf "%s = %s" target value]
             | otherwise -> [printf "%s" (toPyCode defuse a)]
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

instance MkC Bool where
  mkc = PyBool

updateL :: [a] -> Int -> (a -> a) -> [a]
updateL (x:xs) 0 f = f x : xs
updateL (x:xs) n f = x : updateL xs n f
updateL _ _ _      = error "invalid index"

indentInc :: Int -> PyMState ()
indentInc n = do
  st <- get
  if  | n == 0 -> return ()
      | n < 0  -> put $ st {layout = drop (-n) $ layout st}
      | n > 0  -> put $ st {layout = replicate n ' ' ++ layout st}

indent = indentInc 2
dedent = indentInc (-2)

appendCode :: (DefUse -> [String]) -> PyMState ()
appendCode f = state $ \st ->
  ((), st {code = \defuse -> map (layout st ++) (f defuse) ++ code st defuse})

pushCode :: (DefUse -> String) -> PyMState ()
pushCode f = state $ \st ->
  ((), st {code = \defuse -> (layout st ++ f defuse) : code st defuse})


assign :: PyExp a -> PyExp a -> PyMState ()
assign target value = appendCode $ \defuse ->
  let target' = toPyCode defuse target
      value'  = toPyCode defuse value
  in
  case target of
    LS i | isUsed i defuse -> [printf "%s = %s" target' value']
    LS _                   -> [value']
    GS _                   -> [printf "%s = %s" target' value']
    _                      -> error "invalid assignments"

cond :: PyExp Bool -> PyMState (PyExp a) -> PyMState (PyExp a) -> PyMState (PyExp a)
cond expr arm1 arm2 = do
  test <- use expr
  ret_slot <- allocate_id "if"
  let ret = LS ret_slot
  pushCode $ \defuse -> (printf "if %s:" $ toPyCode defuse test)
  indent
  arm1 <- arm1
  assign ret arm1
  dedent
  pushCode $ \defuse -> "else:"
  indent
  arm2 <- arm2
  assign ret arm2
  dedent
  return ret

call :: PyExp (a -> b) -> PyExp a -> PyMState (PyExp b)
call f a = do
  f <- use f
  a <- use a
  return $ PyCall f a

generate :: PyMState a -> (a, String)
generate m =
  let (a, PyState {code, vregs}) = runState m emptyPyState
  in (a, intercalate "\n" $ reverse $ code vregs)

class PIter a e | a -> e where
  getIter :: PyExp a -> PyExp a
  getIter = id
instance PIter (PList e) e

instance PIter (PSet e) e

each :: PIter a e => PyExp a -> (PyExp e -> PyMState any) -> PyMState ()
each iterable applyEach = do
  iterVar <- mksymbol "iter"
  iterable <- use (getIter iterable)
  pushCode $ \defuse ->
    printf "for %s in %s:"
           (toPyCode defuse iterVar)
           (toPyCode defuse iterable)
  indent
  applyEach iterVar
  dedent

class PContainer a e | a -> e where
  pyIn :: PyExp e -> PyExp a -> PyExp Bool
  pyIn = PyIn

class PSequence a i e | a i -> e where
  pyIndex :: PyExp a -> PyExp i -> PyExp e
  pyIndex = PyIndex

