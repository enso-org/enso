{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Control.Lens.Utils.Nested where

import Prelude hiding (mempty)
import Control.Lens
import Control.Monad
import Data.List.NonEmpty
import Data.Monoids
import Data.Default
import Data.Maybe
import qualified Data.Map as Map
import           Data.Map (Map)

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
          insertNewNested _   (t :| [])       v = at t .~ Just v
          insertNewNested def (t :| (e : es)) v = at t .~ Just (insertNewNested def (e :| es) v def)


nestedAt' :: (NestedAtCtx a, Mempty (IxValue a)) => NonEmpty (Index a) -> Lens' a (IxValue a)
nestedAt' ixs = lens (fromMaybe mempty . view (nestedAt ixs)) (\a -> flip (set (nestedAt ixs)) a . Just)


nestedIx :: NestedIxedCtx a => NonEmpty (Index a) -> Traversal' a (IxValue a)
nestedIx = \case
    (t :| [])     -> ix t
    (t :| (i:is)) -> ix t . nestedIx (i :| is)

emptyMap :: Prism' (Map k a) ()
emptyMap = prism' (\() -> Map.empty) $ guard . Map.null

subMapAt :: (Functor f, Ord p) => p -> (Maybe a -> f (Maybe a))
    -> Maybe (Map p a) -> f (Maybe (Map p a))
subMapAt t = non' emptyMap . at t

at' :: (Functor f, Ord p) => p -> (Maybe a -> f (Maybe a)) -> Maybe (Map p a)
    -> f (Maybe (Map p a))
at' = subMapAt -- FIXME[WD]: make an abstraction with `Empty` class which do not have to be monoid

-- | Warning! This function does not hold lens laws: deleting and re-adding a key destroys everything appart of `a`,
--            so `set l (Just x) (set l Nothing s) /= set l (Just x) s`
unsafeLensedMapAt :: (Ord k, Default v) => k -> Lens' v a -> Lens' (Map k v) (Maybe a)
unsafeLensedMapAt k l f m = f ma <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) ma
    Just a' -> Map.alter (Just . set l a' . fromMaybe def) k m
    where ma = view l <$> Map.lookup k m

