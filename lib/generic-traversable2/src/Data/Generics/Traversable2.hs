{-# LANGUAGE NoMonoLocalBinds        #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Generics.Traversable2
    (module Data.Generics.Traversable2, module X) where
import Data.Generics.Traversable2.Class     as X
import Data.Generics.Traversable2.Instances ()

import qualified Prelude  as P
import           Prologue hiding (Foldable, Traversable, foldr, mapM, traverse)

import qualified Control.Monad.State.Strict as State

import Control.Applicative        (WrappedMonad (WrapMonad), unwrapMonad)
import Control.Monad.State.Strict (StateT)
import Data.Functor.Identity      (Identity (Identity), runIdentity)
import GHC.Exts                   (Constraint)



-----------------
-- === Map === --
-----------------

-- === Definition === --

class MapElem t a where
    mapElem :: a -> a
    mapElem = id
    {-# INLINE mapElem #-}

map  :: ∀ t a.    Map  t a => a    -> a
map1 :: ∀ t a t1. Map1 t a => a t1 -> a t1
map  = runIdentity . traverse  @(Map__ t) ; {-# INLINE map  #-}
map1 = runIdentity . traverse1 @(Map__ t) ; {-# INLINE map1 #-}


-- === Implementation === --

data Map__ t
type instance Result (Map__ t) = Identity

type Map  t = Traversable  (Map__ t)
type Map1 t = Traversable1 (Map__ t)

instance MapElem t a
      => TraverseElem (Map__ t) a where
    traverseElem = pure . mapElem @t
    {-# INLINE traverseElem #-}



------------------
-- === MapM === --
------------------

-- === Definition === --

class MapElemM t m a where
    mapElemM :: a -> m a

    default mapElemM :: Applicative m => a -> m a
    mapElemM = pure
    {-# INLINE mapElemM #-}

mapM   :: ∀ t a m.    MapM  t m a => a    -> m a
mapM_  :: ∀ t a m.    MapM  t m a => a    -> m ()
mapM1  :: ∀ t a m t1. MapM1 t m a => a t1 -> m (a t1)
mapM1_ :: ∀ t a m t1. MapM1 t m a => a t1 -> m ()
mapM   = traverse  @(MapM__ t m) ; {-# INLINE mapM   #-}
mapM1  = traverse1 @(MapM__ t m) ; {-# INLINE mapM1  #-}
mapM_  = void . mapM  @t         ; {-# INLINE mapM_  #-}
mapM1_ = void . mapM1 @t         ; {-# INLINE mapM1_ #-}


-- === Implementation === --

data MapM__ t (m :: Type -> Type)
type instance Result (MapM__ t m) = m

type MapM  t m a = (Traversable  (MapM__ t m) a, Applicative m)
type MapM1 t m a = (Traversable1 (MapM__ t m) a, Applicative m)

instance MapElemM t m a
      => TraverseElem (MapM__ t m) a where
    traverseElem = mapElemM @t
    {-# INLINE traverseElem #-}



------------------
-- === Fold === --
------------------

-- === Definition === --

class FoldElem t a r where
    foldElem :: a -> r -> r
    foldElem = \_ -> id
    {-# INLINE foldElem #-}

type Foldl t r = Traversable (Foldl__ t r)
type Foldr t r = Traversable (Foldr__ t r)

foldl :: ∀ t r a. Foldl t r a => r -> a -> r
foldr :: ∀ t r a. Foldr t r a => r -> a -> r
foldl = \r a -> appEndoFold (traverse @(Foldl__ t r) a) r
foldr = \r a -> appEndoFold (traverse @(Foldr__ t r) a) r
{-# INLINE foldl #-}
{-# INLINE foldr #-}


-- === Implementation === --

data Foldl__ t r
data Foldr__ t r
type instance Result (Foldl__ t r) = EndoFoldl r
type instance Result (Foldr__ t r) = EndoFoldr r

instance FoldElem t a r
      => TraverseElem (Foldl__ t r) a where
    traverseElem = EndoFold . foldElem @t
    {-# INLINE traverseElem #-}

instance FoldElem t a r
      => TraverseElem (Foldr__ t r) a where
    traverseElem = EndoFold . foldElem @t
    {-# INLINE traverseElem #-}


-- === Fold endomorphisms === --

data EndoFoldLeft
data EndoFoldRight

newtype EndoFold s r a = EndoFold { appEndoFold :: r -> r }
type    EndoFoldl = EndoFold EndoFoldLeft
type    EndoFoldr = EndoFold EndoFoldRight

instance Functor (EndoFold s r) where
    fmap = \_ a -> coerce (coerce a :: r -> r)
    {-# INLINE fmap #-}

instance Applicative (EndoFold EndoFoldLeft r) where
    pure = \_ -> EndoFold id
    {-# INLINE pure #-}

    (<*>) = coerce ((\f g a -> g $! f a) :: (r -> r) -> (r -> r) -> (r -> r))
    {-# INLINE (<*>) #-}

instance Applicative (EndoFold EndoFoldRight r) where
    pure = \_ -> EndoFold id
    {-# INLINE pure #-}

    (<*>) = coerce ((\f g a -> f $! g a) :: (r -> r) -> (r -> r) -> (r -> r))
    {-# INLINE (<*>) #-}




-------------------
-- === FoldM === --
-------------------

-- === Definition === --

class FoldElemM t a m r where
    foldElemM :: a -> r -> m r

    default foldElemM :: Applicative m => a -> r -> m r
    foldElemM = \_ -> pure
    {-# INLINE foldElemM #-}

type FoldlM t m r a = (Traversable (FoldlM__ t m r) a, Monad m)

foldlM :: ∀ t r a m. FoldlM t m r a => r -> a -> m r
foldlM = \r a -> appEndoFoldM (traverse @(FoldlM__ t m r) a) r
{-# INLINE foldlM #-}


-- === Implementation === --

data FoldlM__ t (m :: Type -> Type) r
type instance Result (FoldlM__ t m r) = EndoFoldlM m r

instance (FoldElemM t a m r, Functor m)
      => TraverseElem (FoldlM__ t m r) a where
    traverseElem = endoFoldM (foldElemM @t)
    {-# INLINE traverseElem #-}


-- === Fold endomorphisms === --


newtype EndoFoldM s m r a = EndoFoldM (StateT r m a)
    deriving (Functor, Applicative, Monad, MonadIO)

type EndoFoldlM = EndoFoldM EndoFoldLeft

endoFoldM :: Functor m => (a -> r -> m r) -> a -> EndoFoldM s m r a
endoFoldM = \f a -> EndoFoldM $! State.StateT $! \r -> (a,) <$> f a r
{-# INLINE endoFoldM #-}

appEndoFoldM :: Monad m => EndoFoldM s m r a -> (r -> m r)
appEndoFoldM = \(EndoFoldM s) -> State.execStateT s
{-# INLINE appEndoFoldM #-}




--------------------------------- Examples -------------------------------------


--------------------------
-- === ToListByType === --
--------------------------

-- === Definition === --

type ToListByType t = Foldr (ToListByType__ t) [t]

toListByType :: ∀ t a. ToListByType t a => a -> [t]
toListByType = foldr @(ToListByType__ t) mempty
{-# INLINE toListByType #-}


-- === Implementation === --

data ToListByType__ a

instance {-# OVERLAPPABLE #-} (lst ~ [a])
      => FoldElem (ToListByType__ a) b lst where
    foldElem = \_ -> id
    {-# INLINE foldElem #-}

instance (lst ~ [a]) => FoldElem (ToListByType__ a) a lst where
    foldElem = (:)
    {-# INLINE foldElem #-}

