{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Class where

import Luna.Prelude hiding (head, tail)

import           Data.RTuple (List)
import qualified Data.RTuple as List

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.State      (StateT)
import           Control.Monad.Primitive

import           Luna.IR.Internal.IR   (Key, IRMonad, HasIdx, KeyReadError, KeyMissingError, KeyData)
import qualified Luna.IR.Internal.IR   as IR
import           Luna.IR.Term.Layout.Class (Abstract)
import           Type.Maybe                (FromJust)

import Luna.IR.Layer
import Type.List (In)




---------------------
-- === KeyList === --
---------------------

newtype KeyList lst = KeyList (List (Key <$> lst))
makeWrapped ''KeyList


prepend :: Key k -> KeyList ks -> KeyList (k ': ks)
prepend k = wrapped %~ List.prepend k ; {-# INLINE prepend #-}

-- | FIXME[WD]: tail cannot be constructed as wrapped . List.tail . Why?
tail :: Lens' (KeyList (k ': ks)) (KeyList ks)
tail = lens (wrapped %~ (view List.tail)) $ flip (\lst -> wrapped %~ (List.tail .~ unwrap' lst)) ; {-# INLINE tail #-}

head :: Lens' (KeyList (k ': ks)) (Key k)
head = wrapped . List.head ; {-# INLINE head #-}



-------------------------------


-- === Properties === --

type family Inputs    pass :: [*]
type family Outputs   pass :: [*]
type family Elements  pass :: [*]
type family Preserves pass :: [*]


-- === Data declarations ===

type    Pass  pass m   = PassT pass m ()
newtype PassT pass m a = PassT (StateT (State pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)


type KeyTypes pass = Inputs pass <> Outputs pass -- FIXME (there are duplicates in the list)
type Keys     pass = KeyTypes pass
type State    pass = KeyList (KeyTypes pass)

type        GetState m = State (GetPass m)
type family GetPass  m where
    GetPass (PassT pass m) = pass
    GetPass (t m)          = GetPass m

makeWrapped ''PassT


-- === Utils ===

initPass :: (LookupKeys n (Keys pass), Monad m) => PassT pass m a -> n (Either Err (m a))
initPass p = return . fmap (State.evalStateT (unwrap' p)) =<< lookupKeys ; {-# INLINE initPass #-}

eval :: LookupKeys m (Keys pass) => PassT pass m a -> m (Either Err a)
eval = join . fmap sequence . initPass ; {-# INLINE eval #-}

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
class    Monad m             => LookupKeys m keys      where lookupKeys :: m (Either Err (KeyList keys))
instance Monad m             => LookupKeys m '[]       where lookupKeys = return $ return (wrap' List.empty)
instance ReLookupKeys k m ks => LookupKeys m (k ': ks) where lookupKeys = prepend <<$>> (justErr (Err $ typeRep (Proxy :: Proxy k)) <$> IR.uncheckedLookupKey)
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


-- === Readable === --

class    Monad m                                => Readable k m     where getKey :: m (Key k)
instance {-# OVERLAPPABLE #-} SubReadable k t m => Readable k (t m) where getKey = lift getKey     ; {-# INLINE getKey #-}
type SubReadable k t m = (Readable k m, MonadTrans t, Monad (t m))

instance ( Monad m
         , ContainsKey k (Keys pass)
         , Assert (k `In` (Inputs pass)) (KeyReadError k))
      => Readable k (PassT pass m) where getKey = findKey <$> get ; {-# INLINE getKey #-}


-- === ContainsKey === --

class                                     ContainsKey k ls        where findKey :: KeyList ls -> Key k
instance {-# OVERLAPPING #-}              ContainsKey k (k ': ls) where findKey = view head           ; {-# INLINE findKey #-}
instance ContainsKey k ls              => ContainsKey k (l ': ls) where findKey = findKey . view tail ; {-# INLINE findKey #-}
instance TypeError (KeyMissingError k) => ContainsKey k '[]       where findKey = impossible          ; {-# INLINE findKey #-}




------------

readLayer :: forall layer t m. (IRMonad m, HasIdx t, Readable (Layer (Abstract t) layer) m ) => t -> m (LayerData layer t)
readLayer t = flip IR.unsafeReadLayer t =<< getKey @(Layer (Abstract t) layer)
