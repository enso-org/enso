{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Class where

import Luna.Prelude

import           Data.RTuple (List)
import qualified Data.RTuple as List

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.State      (StateT)
import           Control.Monad.Primitive

import           Luna.IR.Internal.IR   (Key, IOAccess(..), IRMonad, HasIdx, AssertLayerReadable, AssertLayerReadable', AssertKeyReadable, KeyData)
import qualified Luna.IR.Internal.IR   as IR
import           Luna.IR.Term.Layout.Class (Abstract)
import           Type.Maybe                (FromJust)

import Luna.IR.Layer


-- === Properties === --

type family Keys      pass :: [*]
type family Elements  pass :: [*]
type family Preserves pass :: [*]


-- === Data declarations ===

type    Pass  pass m   = PassT pass m ()
newtype PassT pass m a = PassT (StateT (State pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

type State pass = List (Keys pass)

type        GetKeys  m = Keys  (GetPass m)
type        GetState m = State (GetPass m)
type family GetPass  m where
    GetPass (PassT pass m) = pass
    GetPass (t m)          = GetPass m

makeWrapped ''PassT


-- === Utils ===

-- eval :: Monad m => PassT pass m a -> List (Keys pass) -> m a
-- eval = State.evalStateT . unwrap' ; {-# INLINE eval #-}

-- eval :: Monad m => PassT pass m a -> List (Keys pass) -> m a
-- eval p = State.evalStateT (unwrap' p) =<< lookupKeys ; {-# INLINE eval #-}
--
--          a -> m b        ->   m (f a)  -> m (f b)

initPass :: (LookupKeys n (Keys pass), Monad m) => PassT pass m a -> n (Either Err (m a))
initPass p = return . fmap (State.evalStateT (unwrap' p)) =<< lookupKeys ; {-# INLINE initPass #-}

eval :: LookupKeys m (Keys pass) => PassT pass m a -> m (Either Err a)
eval = join . fmap sequence . initPass ; {-# INLINE eval #-}


-- eval :: Pass pass a -> List (Keys pass) -> a
-- eval  = runIdentity .: evalT ; {-# INLINE eval #-}

with :: MonadPass m => (GetState m -> GetState m) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadPass m => (GetState m -> (GetState m, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a
{-# INLINE modify #-}

modify_ :: MonadPass m => (GetState m -> GetState m) -> m ()
modify_ = modify . fmap (,())
{-# INLINE modify_ #-}


-- === Keys lookup === --


data Err = Err TypeRep deriving (Show)

type ReLookupKeys k m ks = (IR.KeyMonad k m, LookupKeys m ks, Typeable k)
class    Monad m             => LookupKeys m keys              where lookupKeys :: m (Either Err (List keys))
instance Monad m             => LookupKeys m '[]               where lookupKeys = return $ return List.empty
instance ReLookupKeys k m ks => LookupKeys m (Key acc k ': ks) where lookupKeys = List.prepend <<$>> (justErr (Err $ typeRep (Proxy :: Proxy k)) <$> IR.uncheckedLookupKey)
                                                                                               <<*>> lookupKeys


infixl 4 <<*>>
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = (<*>) . fmap (<*>) ; {-# INLINE (<<*>>) #-}



-- === MonadPass === --


class Monad m => MonadPass m where
    get :: m (GetState m)
    put :: GetState m -> m ()

instance Monad m => MonadPass (PassT pass m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadPass m, MonadTrans t, Monad (t m),GetState m ~ GetState (t m)) => MonadPass (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}


-- === Instances === --

-- Transformats

instance State.MonadState s m => State.MonadState s (PassT pass m) where
    get = wrap' $ lift   State.get ; {-# INLINE get #-}
    put = wrap' . lift . State.put ; {-# INLINE put #-}

-- Primitive

instance PrimMonad m => PrimMonad (PassT pass m) where
    type PrimState (PassT pass m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}


-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

-- If we ever think that there should exist more Key providers than passes
-- or pure functions, we can convert this family to open one and create
-- Accesible instances for every needed monad. However, this design simplifies
-- code a lot for now.
type family KeyAccess k (m :: * -> *) :: IOAccess where
    KeyAccess k (PassT pass m) = MatchKeyType k (Keys pass)
    KeyAccess k (t m)          = KeyAccess k m


type Key' k m = Key (KeyAccess k m) k


class    Monad m                                  => Accessible k m              where getKey :: m (Key' k m)
instance (KeyProvider k (Keys pass), Monad m)     => Accessible k (PassT pass m) where getKey = findKey <$> get ; {-# INLINE getKey #-}
instance {-# OVERLAPPABLE #-} SubAccessible k t m => Accessible k (t m)          where getKey = lift getKey     ; {-# INLINE getKey #-}
type SubAccessible k t m = (Accessible k m, KeyAccess k (t m) ~ KeyAccess k m, MonadTrans t, Monad (t m))


-- === KeyProvider === --

type        MatchKey     key lst = Key (MatchKeyType key lst) key
type family MatchKeyType key lst where
    MatchKeyType k (Key s k ': ls) = s
    MatchKeyType k (l       ': ls) = MatchKeyType k ls

type SubAccess k l ls = (MatchKey k ls ~ MatchKey k (l ': ls), KeyProvider k ls)

class                        KeyProvider k ls              where findKey :: List ls -> MatchKey k ls
instance SubAccess k l ls => KeyProvider k (l       ': ls) where findKey = findKey . view List.tail ; {-# INLINE findKey #-}
instance {-# OVERLAPPING #-} KeyProvider k (Key s k ': ls) where findKey = view List.head             ; {-# INLINE findKey #-}




------------


readLayer :: forall layer t m. (IRMonad m, HasIdx t, Readable (Layer (Abstract t) layer) m ) => t -> m (LayerData layer t)
readLayer t = flip IR.unsafeReadLayer t =<< getKey @(Layer (Abstract t) layer)

-- IR.unsafeReadLayer - jest zwiazane zz odcyztywaniem layeru, zmienic nazwe
readKey :: Readable k m => m (Key' k m)
readKey = getKey ; {-# INLINE readKey #-}


class Readable2 k m where
    read2 :: Key' k m -> m (KeyData m k)

class Writable2 k m where
    write2 :: Key' k m -> KeyData m k -> m ()


-- instance Readable2


-- dokonczyc przechdozenie na ladne readkey -> readkey'
-- dodatkowo Accessible powinno sprawdzac czy klucz istnieje
-- jak bedzie dobre readKey to zrobic ReadKey dla dowolnego elementu i newElem przeniesc na readKey


type Readable k m = (Accessible k m, AssertReadable k m)

class                                                        AssertReadable k (m :: * -> *)
instance AssertKeyReadable (KeyAccess k (PassT pass m)) k => AssertReadable k (PassT pass m)
instance {-# OVERLAPPABLE #-} AssertReadable k m          => AssertReadable k (t m)

-- type family AssertReadable key m :: Constraint where
--     AssertReadable I m  = ()
--     AssertReadable k IM = ()
--     AssertReadable k m  = AssertKeyReadable (KeyAccess k m) k

-- read2 :: forall t layer m abs. (IRMonad m, abs ~ Abstract t, AssertLayerReadable (KeyAccess (Layer abs layer) m) abs layer)
--       => t -> m (LayerData layer t)
-- read2 t = flip readKey t =<< getKey @(Layer abs layer)


-- type AssertLayerReadable a t l = Assert (Readable a) (LayerReadError  t l)
--
-- (Type.Error.Assert
--                           (IR.Readable
--                              (FromJust (MatchKeyType (Layer abs layer) (Keys pass))))
--                           (IR.LayerReadError abs layer))
--
-- readKey :: (IRMonad m, HasIdx t, AssertLayerReadable acc (Abstract t) layer)
--         => LayerKey acc (Abstract t) layer -> t -> m (LayerData layer t)

-- Key () key
-- acc ~ MatchKeyType key (GetKeys m)
