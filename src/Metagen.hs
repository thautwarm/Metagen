module Metagen
    ( main
    ) where

import           Metagen.Infr
import           Metagen.Python
import qualified Metagen.Python.Prelude as Py
import           Metagen.Templates

testPy = do
    let xs = PyList [mkc True, mkc False]
    each xs $ \v -> cond v
        do
          call Py.print (PyStr "hello true")
        do
          call Py.print (PyStr "hello false")

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
  putStrLn $ snd (generate testPy)
