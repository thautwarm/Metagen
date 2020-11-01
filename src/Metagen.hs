module Metagen
    ( main
    ) where

import           Metagen.Infr
import           Metagen.Templates
import           Metagen.Python


testPy = do
    cond (PyBool True)
        do
            call py_print (PyStr "hello true")
        do
            call py_print (PyStr "hello false")

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
