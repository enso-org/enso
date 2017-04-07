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
import           Data.List.NonEmpty (NonEmpty ((:|)))



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


type NestedCtx     a = (Index a ~ Index (IxValue a), IxValue a ~ IxValue (IxValue a))
type NestedAtCtx   a = (NestedCtx a, At   a, At   (IxValue a))
type NestedIxedCtx a = (NestedCtx a, Ixed a, Ixed (IxValue a))

nestedAt :: (NestedAtCtx a, Mempty (IxValue a)) => NonEmpty (Index a) -> Lens' a (Maybe (IxValue a))
nestedAt = nestedDefAt mempty

nestedDefAt :: NestedAtCtx a => IxValue a -> NonEmpty (Index a) -> Lens' a (Maybe (IxValue a))
nestedDefAt def p = case p of
    (t :| [])       -> at t
    (t :| (e : es)) -> lens (join . fmap (view (nestedDefAt def $ e :| es)) . view (at t))
                     $ \h mv -> h & case h ^. at t of
                                    Just _  -> ix t %~ (nestedDefAt def (e :| es) .~ mv)
                                    Nothing -> maybe id (insertNewNested def p) mv
    where insertNewNested :: NestedAtCtx a => IxValue a -> NonEmpty (Index a) -> IxValue a -> (a -> a)
          insertNewNested def (t :| [])       v = at t .~ Just v
          insertNewNested def (t :| (e : es)) v = at t .~ Just (insertNewNested def (e :| es) v def)


nestedAt' :: (NestedAtCtx a, Mempty (IxValue a)) => NonEmpty (Index a) -> Lens' a (IxValue a)
nestedAt' ixs = lens (fromMaybe mempty . view (nestedAt ixs)) (\a -> flip (set (nestedAt ixs)) a . Just)


nestedIx :: NestedIxedCtx a => NonEmpty (Index a) -> Traversal' a (IxValue a)
nestedIx = \case
    (t :| [])     -> ix t
    (t :| (i:is)) -> ix t . nestedIx (i :| is)

emptyMap :: Prism' (Map k a) ()
emptyMap = prism' (\() -> Map.empty) $ guard . Map.null

subMapAt t = non' emptyMap . at t

at' = subMapAt -- FIXME[WD]: make an abstraction with `Empty` class which do not have to be monoid
