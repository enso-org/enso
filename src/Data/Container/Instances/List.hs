{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Container.Instances.List () where

import Prelude

import Data.Container.Class
import Data.Container.List
import Data.Container.Opts
import Data.Container.Proxy
import Data.Container.Utils
import Data.Proxy

import qualified Data.List as List

------------------------------
-- === Global instances === --
------------------------------

type instance Index     [a] = Int
type instance Item      [a] = a
type instance Container [a] = [a]
type instance DataStore [a] = [a]

instance Monad m => IsContainerM  m [a] where
    fromContainerM = return       ; {-# INLINE fromContainerM #-}
instance Monad m => HasContainerM m [a] where
    viewContainerM = return       ; {-# INLINE viewContainerM #-}
    setContainerM  = const.return ; {-# INLINE setContainerM  #-}

instance ToList   [a] where toList = id   ; {-# INLINE toList   #-}
instance FromList [a] where fromList = id ; {-# INLINE fromList #-}

----------------------------------
-- === Operations instances === --
----------------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp [a] = '[]
type instance ModsOf   MeasurableOp [a] = '[]

type instance ParamsOf MinBoundedOp [a] = '[]
type instance ModsOf   MinBoundedOp [a] = '[]

type instance ParamsOf MaxBoundedOp [a] = '[]
type instance ModsOf   MaxBoundedOp [a] = '[]

instance Monad m => MeasurableQM_ '[] '[] m [a] where
    sizeM_ _ = return . Res () . length ; {-# INLINE sizeM_ #-}

instance (Monad m, idx ~ Int) => MinBoundedQM_ '[] '[] m idx [a] where
    minBoundM_ _ _ = return $ Res () 0  ; {-# INLINE minBoundM_ #-}

instance (Monad m, idx ~ Int) => MaxBoundedQM_ '[] '[] m idx [a] where
    maxBoundM_ _ = return . Res () . pred . length ; {-# INLINE maxBoundM_ #-}

-- === Construction ===

-- [+] Singleton
-- [-] Allocable
-- [-] Expandable
-- [-] Growable

type instance ParamsOf SingletonOp [a] = '[]
type instance ModsOf   SingletonOp [a] = '[Ixed]

instance (Monad m, a ~ a') => SingletonQM_ '[N] '[] m a' [a] where
    singletonM_ _ = return . Res () . (: []) ; {-# INLINE singletonM_ #-}


-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable
-- [+] Insertable
-- [-] Freeable

type instance ParamsOf AppendableOp  [a] = '[]
type instance ModsOf   AppendableOp  [a] = '[Ixed]

type instance ParamsOf PrependableOp [a] = '[]
type instance ModsOf   PrependableOp [a] = '[Ixed]

type instance ParamsOf AddableOp     [a] = '[]
type instance ModsOf   AddableOp     [a] = '[Ixed]

type instance ParamsOf RemovableOp   [a] = '[Try]
type instance ModsOf   RemovableOp   [a] = '[Ixed]

type instance ParamsOf InsertableOp  [a] = '[Try]
type instance ModsOf   InsertableOp  [a] = '[Ixed]


instance (Monad m, a ~ a') => AppendableQM_ '[N     ] ps m a' [a] where
    appendM_ _ el l = return . Res ()             $ l ++ [el] ; {-# INLINE appendM_ #-}
instance (Monad m, a ~ a') => AppendableQM_ '[P Ixed] ps m a' [a] where
    appendM_ _ el l = return . Res (length l, ()) $ l ++ [el] ; {-# INLINE appendM_ #-}


instance (Monad m, a ~ a') => PrependableQM_ '[N     ] ps m a' [a] where
    prependM_ _ = return . Res ()      .: (:) ; {-# INLINE prependM_ #-}
instance (Monad m, a ~ a') => PrependableQM_ '[P Ixed] ps m a' [a] where
    prependM_ _ = return . Res (0, ()) .: (:) ; {-# INLINE prependM_ #-}


instance (Monad m, a ~ a') => AddableQM_ '[N     ] ps m a' [a] where
    addM_ = prependM_ ; {-# INLINE addM_ #-}
instance (Monad m, a ~ a') => AddableQM_ '[P Ixed] ps m a' [a] where
    addM_ = prependM_ ; {-# INLINE addM_ #-}



removeElement :: (Monad m, Eq a) => a -> [a] -> m (Int, [a])
removeElement el l = case b of
    []       -> fail "Element not found"
    (_ : b') -> return (length a, a ++ b')
    where (a, b) = List.break (/= el) l

instance (Monad m, Eq a, a ~ a') => RemovableQM_ '[N]      '[P Try] m a' [a] where
    removeM_ _ el l = Res () . snd <$> removeElement el l ; {-# INLINE removeM_ #-}
instance (Monad m, Eq a, a ~ a') => RemovableQM_ '[P Ixed] '[P Try] m a' [a] where
    removeM_ _ el l = do
        (ix, l') <- removeElement el l
        return . Res (ix, ()) $ l'
    {-# INLINE removeM_ #-}



insertElement :: Monad m => Int -> a -> [a] -> m [a]
insertElement ix el l = case b of
    []       -> fail "Index out of bounds"
    (_ : b') -> return $ a ++ (el : b')
    where (a, b) = List.splitAt ix l

instance (Monad m, a ~ a', idx ~ Int) => InsertableQM_ '[N] '[P Try] m idx a' [a] where
    insertM_ _ ix el l = Res ()       <$> insertElement ix el l ; {-# INLINE insertM_ #-}
instance (Monad m, a ~ a', idx ~ Int) => InsertableQM_ '[P Ixed] '[P Try] m idx a' [a] where
    insertM_ _ ix el l = Res (ix, ()) <$> insertElement ix el l ; {-# INLINE insertM_ #-}



-- === Indexing ===

-- [+] Indexable
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes
-- [+] TracksIxes
-- [+] TracksElems

type instance ParamsOf IndexableOp   [a] = '[Unchecked, Try]
type instance ModsOf   IndexableOp   [a] = '[Ixed]

type instance ParamsOf TracksIxesOp  [a] = '[]
type instance ModsOf   TracksIxesOp  [a] = '[]

type instance ParamsOf TracksElemsOp [a] = '[]
type instance ModsOf   TracksElemsOp [a] = '[]

checkBounds :: Int -> [a] -> (String -> b) -> b -> b
checkBounds ix l fail ok = if ix >= length l || ix < 0
    then fail "Index out of bounds"
    else ok

getElement :: (CondOpt unchecked, Monad m) => Proxy unchecked -> Int -> [a] -> (String -> m a) -> m a
getElement unchecked ix l fail = ifOpt unchecked (return $ l !! ix) (checkBounds ix l fail (return $ l !! ix))

getElementChecked :: (CondOpt unchecked, CondOpt try, Monad m) => Proxy unchecked -> Proxy try -> Int -> [a] -> m a
getElementChecked unchecked try idx l = getElement unchecked idx l (ifOpt try fail error)

instance (Monad m, a ~ a', idx ~ Int, CondOpt unchecked, CondOpt try) => IndexableQM_ '[N] '[unchecked, try] m idx a' [a] where
    indexM_ _ idx l = Res ()        <$> getElementChecked (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx l
    {-# INLINE indexM_ #-}
instance (Monad m, a ~ a', idx ~ Int, CondOpt unchecked, CondOpt try) => IndexableQM_ '[P Ixed] '[unchecked, try] m idx a' [a] where
    indexM_ _ idx l = Res (idx, ()) <$> getElementChecked (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx l
    {-# INLINE indexM_ #-}


instance (Monad m, ix ~ Int) => TracksIxesQM_ '[] '[] m ix [a] where
    ixesM_ _ l = return . Res () $ [0 .. length l - 1] ; {-# INLINE ixesM_ #-}


instance (Monad m, a ~ a') => TracksElemsQM_   '[] '[] m a' [a] where
    elemsM_ _ l = return . Res () $ l ; {-# INLINE elemsM_ #-}
