module Control.Lens.Utils (module Control.Lens.Utils, module X) where

import Control.Lens               as X
import Control.Lens.Wrapped.Utils as X


import Data.Monoid
import Control.Lens.TH     (LensRules)
import Language.Haskell.TH (Name, DecsQ, nameBase, mkName)
import Data.Char           (toLower)
import Control.Lens.Internal.FieldTH (_fieldToDef)


makePfxLenses :: Name -> DecsQ
makePfxLenses = makeLensesWith (lensRules {_fieldToDef = typePrefixNamer})

typePrefixNamer :: FieldNamer
typePrefixNamer tn _ n = case nb of
    '_':_ -> [TopName . mkName $ ltn <> nb]
    _     -> []
    where nb     = nameBase n
          (s:ss) = nameBase tn
          ltn    = toLower s : ss
