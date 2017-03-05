{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Control.Lens.Utils (module Control.Lens.Utils, module X) where

import Prelude hiding (mempty)

import Control.Lens               as X hiding (makeLenses, makeClassy)
import Control.Lens.Wrapped.Utils as X

import Data.Maybe          (fromMaybe)
import Control.Monad       (join)
import Data.Monoids
import Control.Lens.TH     (LensRules)
import Language.Haskell.TH (Name, DecsQ, nameBase, mkName, Dec(NewtypeD), Info(TyConI), reify)
import Data.Char           (toLower)
import Control.Lens.Internal.FieldTH (_fieldToDef)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Monad (guard)

makePfxLenses :: Name -> DecsQ
makePfxLenses = makeLensesWith (lensRules {_fieldToDef = typePrefixNamer})



makePfxClassy :: Name -> DecsQ
makePfxClassy = makeLensesWith (classyRules {_fieldToDef = typePrefixNamer})

typePrefixNamer :: FieldNamer
typePrefixNamer tn _ n = case nb of
    '_':_ -> [TopName . mkName $ ltn <> nb]
    _     -> []
    where nb     = nameBase n
          (s:ss) = nameBase tn
          ltn    = toLower s : ss


makeLenses :: Name -> DecsQ
makeLenses name = (<>) <$> makeAutoWrapped name <*> makeAutoLenses name

makeClassy :: Name -> DecsQ
makeClassy name = (<>) <$> makeAutoWrapped name <*> makeAutoClassy name

makeAutoWrapped :: Name -> DecsQ
makeAutoWrapped name = reify name >>= \case
    TyConI (NewtypeD {}) -> makeWrapped name
    _                    -> return mempty


makeAutoLenses :: Name -> DecsQ
makeAutoLenses = makeLensesWith (lensRules {_fieldToDef = autoPrefixNamer})

makeAutoClassy :: Name -> DecsQ
makeAutoClassy = makeLensesWith (classyRules {_fieldToDef = autoPrefixNamer})

autoPrefixNamer :: FieldNamer
autoPrefixNamer tn _ n = case nameBase n of
    '_' : '_' : xs -> [TopName . mkName $ toLower t : ts <> ('_' : xs)]
    '_' :  x  : xs -> [TopName . mkName $ toLower x : xs]
    _              -> []
    where (t:ts) = nameBase tn


nestedAt :: (At a, Mempty a, IxValue a ~ a) => [Index a] -> Lens' a (Maybe a)
nestedAt []       = lens Just const
nestedAt [e]      = at e
nestedAt (e : es) = lens (join . fmap (view (nestedAt es)) . view (at e))
                       $ \h mv -> h & case h ^. at e of
                                      Just _  -> ix e %~ (nestedAt es .~ mv)
                                      Nothing -> maybe id (insertNewNested (e:es)) mv
    where insertNewNested :: (At a, Mempty a, IxValue a ~ a) => [Index a] -> a -> (a -> a)
          insertNewNested [e]      v = at e .~ Just v
          insertNewNested (e : es) v = at e .~ Just (insertNewNested es v mempty)
          {-# INLINE insertNewNested #-}
{-# INLINE nestedAt #-}


nestedAt' :: (At a, Mempty a, IxValue a ~ a) => [Index a] -> Lens' a a
nestedAt' ixs = lens (fromMaybe mempty . view (nestedAt ixs)) (\a -> flip (set (nestedAt ixs)) a . Just)
{-# INLINE nestedAt' #-}


emptyMap :: Prism' (Map k a) ()
emptyMap = prism' (\() -> Map.empty) $ guard . Map.null
{-# INLINE emptyMap #-}

subMapAt t = non' emptyMap . at t

at' = subMapAt -- FIXME[WD]: make an abstraction with `Empty` class which do not have to be monoid
