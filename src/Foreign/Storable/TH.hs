module Foreign.Storable.TH where

import Prelude
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics as Generics

import Language.Haskell.TH


deriveStorable :: Name -> Q [Dec]
deriveStorable name = do
    s <- reify name
    runIO $ putStrLn "\n ------------------------------ \n"
    runIO $ print s

    return []
