{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Class where

import Luna.Prelude hiding (head, tail, elem)

import           Data.RTuple (List ((:-:)))
import qualified Data.RTuple as List

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.State      (StateT)
import           Control.Monad.Primitive

import           Luna.IR.Internal.IR   (Key(Key), Readable(..), Writable(..), KeyReadError, KeyMissingError, rebaseKey, RebasedKeyData)
import qualified Luna.IR.Internal.IR   as IR (KeyMonad, uncheckedLookupKey)
import           Luna.IR.Expr.Layout.Class (Abstract)
import           Type.Maybe                (FromJust)

import Luna.IR.Layer
import Type.List (In)
import qualified GHC.Prim as Prim
import Unsafe.Coerce (unsafeCoerce)


---------------------
-- === DataSet === --
---------------------

newtype DataSet  m lst = DataSet (List (Key m <$> lst))
-- type    DataSetM m     = DataSet (PrimState m)
makeWrapped ''DataSet

prepend :: Key m k -> DataSet m ks -> DataSet m (k ': ks)
prepend k = wrapped %~ List.prepend k ; {-# INLINE prepend #-}

-- | FIXME[WD]: tail cannot be constructed as wrapped . List.tail . Why?
tail :: Lens' (DataSet m (k ': ks)) (DataSet m ks)
tail = lens (wrapped %~ (view List.tail)) $ flip (\lst -> wrapped %~ (List.tail .~ unwrap' lst)) ; {-# INLINE tail #-}

head :: Lens' (DataSet m (k ': ks)) (Key m k)
head = wrapped . List.head ; {-# INLINE head #-}



-------------------------------

-- === Errors === --

data InternalError = MissingData TypeRep deriving (Show, Eq)



-- === Properties === --

type family Inputs    pass :: [*]
type family Outputs   pass :: [*]
type family Emitters  pass :: [*]
type family Preserves pass :: [*]


-- === Data declarations ===

type    Pass    pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT (PassDataSet m pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

type DynPass    m   = DynSubPass m ()
data DynSubPass m a = DynSubPass { _repr      :: TypeRep -- FIXME[WD]: why we need all the data below? maybe we could keep it in some pass registry?
                                 , _inputs    :: [TypeRep]
                                 , _outputs   :: [TypeRep]
                                 , _preserves :: [TypeRep]
                                 , _dynEval   :: m (Either InternalError (m a))
                                 }

--FIXME[WD]:
instance Show (DynSubPass m a) where show _ = "DynPass"

type PassData       pass = Inputs pass <> Outputs pass <> Emitters pass -- FIXME (there are duplicates in the list)
type Keys           pass = PassData pass
type PassDataSet  m pass = DataSet m (PassData pass)
-- type PassDataSetM m pass = PassDataSet (PrimState m) pass

type GetPassData m = PassDataSet (GetPassMonad m) (GetPass m)
type family GetPass  m where
    GetPass (SubPass pass m) = pass
    GetPass (t m)            = GetPass m

type family GetPassMonad m where
    GetPassMonad (SubPass pass m) = m
    GetPassMonad (t m)            = GetPassMonad m


makeWrapped ''SubPass
makeLenses  ''DynSubPass


-- === Utils ===

type Commit m pass = ( Monad m
                     , LookupData m (Keys pass)
                     , Typeable  pass
                     , Typeables (Inputs    pass)
                     , Typeables (Outputs   pass)
                     , Typeables (Preserves pass)
                     )

commit :: forall pass m a. Commit m pass => SubPass pass m a -> DynSubPass m a
commit p = DynSubPass
           (typeRep'  @pass)
           (typeReps' @(Inputs    pass))
           (typeReps' @(Outputs   pass))
           (typeReps' @(Preserves pass))
           (initPass p)
{-# INLINE commit #-}

initPass :: (LookupData m (Keys pass), Monad m) => SubPass pass m a -> m (Either InternalError (m a))
initPass p = return . fmap (State.evalStateT (unwrap' p)) =<< lookupData ; {-# INLINE initPass #-}

eval :: Monad m => DynSubPass m a -> m (Either InternalError a)
eval = join . fmap sequence . run ; {-# INLINE eval #-}

eval' :: Commit m pass => SubPass pass m a -> m (Either InternalError a)
eval' = eval . commit ; {-# INLINE eval' #-}

run :: DynSubPass m a -> m (Either InternalError (m a))
run = view dynEval ; {-# INLINE run #-}



-- === Keys lookup === --

type ReLookupData k m ks = (IR.KeyMonad k m, LookupData m ks, Typeable k)
class    Monad m             => LookupData m keys      where lookupData :: m (Either InternalError (DataSet m keys))
instance Monad m             => LookupData m '[]       where lookupData = return $ return (wrap' List.empty)
instance ReLookupData k m ks => LookupData m (k ': ks) where lookupData = prepend <<$>> (justErr (MissingData $ typeRep' @k) <$> IR.uncheckedLookupKey)
                                                                                  <<*>> lookupData


infixl 4 <<*>>
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = (<*>) . fmap (<*>) ; {-# INLINE (<<*>>) #-}



-- === MonadPass === --

class Monad m => MonadPass m where
    get :: m (GetPassData m)
    put :: GetPassData m -> m ()

instance Monad m => MonadPass (SubPass pass m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadPass m, MonadTrans t, Monad (t m),GetPassData m ~ GetPassData (t m)) => MonadPass (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

modifyM :: MonadPass m => (GetPassData m -> m (a, GetPassData m)) -> m a
modifyM f = do
    s       <- get
    (a, s') <- f s
    put s'
    return a
{-# INLINE modifyM #-}

modifyM_ :: MonadPass m => (GetPassData m -> m (GetPassData m)) -> m ()
modifyM_ = modifyM . fmap (fmap ((),)) ; {-# INLINE modifyM_ #-}

modify :: MonadPass m => (GetPassData m -> (a, GetPassData m)) -> m a
modify = modifyM . fmap return ; {-# INLINE modify #-}

modify_ :: MonadPass m => (GetPassData m -> GetPassData m) -> m ()
modify_ = modifyM_ . fmap return ; {-# INLINE modify_ #-}

with :: MonadPass m => (GetPassData m -> GetPassData m) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}



-- === Instances === --

-- MonadTrans
instance MonadTrans (SubPass pass) where
    lift = SubPass . lift ; {-# INLINE lift #-}

-- Transformats
instance State.MonadState s m => State.MonadState s (SubPass pass m) where
    get = wrap' $ lift   State.get ; {-# INLINE get #-}
    put = wrap' . lift . State.put ; {-# INLINE put #-}

-- Primitive
instance PrimMonad m => PrimMonad (SubPass pass m) where
    type PrimState (SubPass pass m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}





-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<


-- Readable
instance ( Monad m
         , ContainsKey k (Keys pass)
         , Assert (k `In` (Inputs pass)) (KeyReadError k)
         , RebasedKeyData (SubPass pass m) m k)
      => Readable k (SubPass pass m) where getKey = rebaseKey . view findKey <$> get ; {-# INLINE getKey #-}

-- Writable
instance ( Monad m
         , ContainsKey k (Keys pass)
         , Assert (k `In` (Inputs pass)) (KeyReadError k)
         , RebasedKeyData (SubPass pass m) m k)
      => Writable k (SubPass pass m) where putKey k = modify_ (findKey .~ rebaseKey k) ; {-# INLINE putKey #-}


-- === ContainsKey === --

class                                     ContainsKey k ls        where findKey :: Lens' (DataSet m ls) (Key m k)
instance {-# OVERLAPPING #-}              ContainsKey k (k ': ls) where findKey = head           ; {-# INLINE findKey #-}
instance ContainsKey k ls              => ContainsKey k (l ': ls) where findKey = tail . findKey ; {-# INLINE findKey #-}
instance TypeError (KeyMissingError k) => ContainsKey k '[]       where findKey = impossible     ; {-# INLINE findKey #-}
-- getKey :: m (Key m k)
