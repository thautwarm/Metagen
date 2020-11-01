module Metagen
    ( main
    ) where

import           Metagen.Infr
import           Metagen.Templates


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
