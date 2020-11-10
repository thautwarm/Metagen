module Metagen.Python.Prelude
  ( print )
where

import           Control.Monad.State.Lazy
import           Data.List                (intercalate)
import           Metagen.Infr
import           Metagen.Python
import           Metagen.Templates
import           Prelude                  (id, ($))
import           Text.Printf              (printf)


print :: forall a. PyExp (a -> ())
print = GS "print"

