module Control.Lens.Utils.TH (makeLenses, makeLenses', makeClassy, makeClassy') where

import Prelude

import Language.Haskell.TH
import Control.Lens                  hiding (makeLenses, makeClassy)
import Control.Lens.Internal.FieldTH (_fieldToDef)
import Data.Char                     (toLower)

makeLenses' :: Name -> DecsQ
makeLenses  :: Name -> DecsQ
makeLenses'     = makeLensesWith (lensRules {_fieldToDef = autoPrefixNamer})
makeLenses name = (<>) <$> makeAutoWrapped name <*> makeLenses' name

makeClassy' :: Name -> DecsQ
makeClassy  :: Name -> DecsQ
makeClassy'     = makeLensesWith (classyRules {_fieldToDef = autoPrefixNamer})
makeClassy name = (<>) <$> makeAutoWrapped name <*> makeClassy' name

makeAutoWrapped :: Name -> DecsQ
makeAutoWrapped name = reify name >>= \case
    TyConI (NewtypeD {}) -> makeWrapped name
    _                    -> return mempty

autoPrefixNamer :: FieldNamer
autoPrefixNamer tn _ n = case nameBase n of
    '_' : '_' : xs -> [TopName . mkName $ toLower t : ts <> ('_' : xs)]
    '_' :  x  : xs -> [TopName . mkName $ toLower x : xs]
    _              -> []
    where (t:ts) = nameBase tn

