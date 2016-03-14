{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Container.SizeTracking where

import Prelude

import Data.Container.Class
import Data.Container.List
import Data.Container.Opts
import Data.Container.Proxy
import Data.Container.Utils
import Data.Proxy

--------------------------
-- === SizeTracking === --
--------------------------

data SizeTracking a = SizeTracking Int !a deriving (Functor, Foldable, Traversable)

type instance Index     (SizeTracking a) = Index a
type instance Item      (SizeTracking a) = Item  a
type instance Container (SizeTracking a) = SizeTracking a
type instance DataStore (SizeTracking a) = DataStore    a

instance Monad m => IsContainerM  m (SizeTracking a) where fromContainerM = return
instance Monad m => HasContainerM m (SizeTracking a) where viewContainerM = return
                                                           setContainerM  = const . return

instance ToList a   => ToList   (SizeTracking a) where
    toList (SizeTracking _ c) = toList c ; {-# INLINE toList #-}
instance FromList a => FromList (SizeTracking a) where
    fromList l = SizeTracking (length l) $ fromList l ; {-# INLINE fromList #-}

-- === Finite ===

-- [+] Measurable
-- [ ] MinBounded
-- [ ] MaxBounded

type instance ParamsOf MeasurableOp (SizeTracking a) = '[]
type instance ModsOf   MeasurableOp (SizeTracking a) = '[]

instance Monad m => MeasurableQM_ '[] '[] m (SizeTracking a) where
    sizeM_ _ (SizeTracking s _) =  return $ Res () s ; {-# INLINE sizeM_ #-}

-- === Modification ===

-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable
-- [ ] Insertable
-- [ ] Freeable

type instance ParamsOf AppendableOp  (SizeTracking a) = ParamsOf AppendableOp a
type instance ModsOf   AppendableOp  (SizeTracking a) = ModsOf   AppendableOp a

type instance ParamsOf PrependableOp (SizeTracking a) = ParamsOf PrependableOp a
type instance ModsOf   PrependableOp (SizeTracking a) = ModsOf   PrependableOp a

type instance ParamsOf AddableOp     (SizeTracking a) = ParamsOf AddableOp a
type instance ModsOf   AddableOp     (SizeTracking a) = ModsOf   AddableOp a

type instance ParamsOf RemovableOp   (SizeTracking a) = ParamsOf RemovableOp a
type instance ModsOf   RemovableOp   (SizeTracking a) = ModsOf   RemovableOp a

-- FIXME[MK->WD]: Research better ways of describing the equality of result types (refactoring the Result_ (...) ~ Result_ (...) constraint)
instance ( Monad m
         , AppendableQM (GetOpts as) (GetOpts ps) m el c
         , Result_ AppendableOp (ElInfo el (Container c)) (GetOpts as) ~ Result_ AppendableOp (ElInfo el (SizeTracking c)) (GetOpts as)
         ) => AppendableQM_ as ps m el (SizeTracking c) where
    appendM_ _ el (SizeTracking s c) = do
        Res extra res <- appendQM (Query :: Query (GetOpts as) (GetOpts ps)) el c
        return $ Res extra $ SizeTracking (succ s) res
    {-# INLINE appendM_ #-}

instance ( Monad m
         , PrependableQM (GetOpts as) (GetOpts ps) m el c
         , Result_ PrependableOp (ElInfo el (Container c)) (GetOpts as) ~ Result_ PrependableOp (ElInfo el (SizeTracking c)) (GetOpts as)
         ) => PrependableQM_ as ps m el (SizeTracking c) where
    prependM_ _ el (SizeTracking s c) = do
        Res extra res <- prependQM (Query :: Query (GetOpts as) (GetOpts ps)) el c
        return $ Res extra $ SizeTracking (succ s) res
    {-# INLINE prependM_ #-}

instance ( Monad m
         , AddableQM (GetOpts as) (GetOpts ps) m el c
         , Result_ AddableOp (ElInfo el (Container c)) (GetOpts as) ~ Result_ AddableOp (ElInfo el (SizeTracking c)) (GetOpts as)
         ) => AddableQM_ as ps m el (SizeTracking c) where
    addM_ _ el (SizeTracking s c) = do
        Res extra res <- addQM (Query :: Query (GetOpts as) (GetOpts ps)) el c
        return $ Res extra $ SizeTracking (succ s) res
    {-# INLINE addM_ #-}

instance ( Monad m
         , RemovableQM (GetOpts as) (GetOpts ps) m el c
         , Result_ RemovableOp (ElInfo el (Container c)) (GetOpts as) ~ Result_ RemovableOp (ElInfo el (SizeTracking c)) (GetOpts as)
         ) => RemovableQM_ as ps m el (SizeTracking c) where
    removeM_ _ el (SizeTracking s c) = do
        Res extra res <- removeQM (Query :: Query (GetOpts as) (GetOpts ps)) el c
        return $ Res extra $ SizeTracking (pred s) res
    {-# INLINE removeM_ #-}
