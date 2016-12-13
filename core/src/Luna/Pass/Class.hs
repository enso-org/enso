{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Class where

import Luna.Prelude hiding (head, tail, elem, repr, Args)

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
import Data.Event (Event)
import Data.TypeVal


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
type family Events    pass :: [*]
type family Preserves pass :: [*]


-- === Data declarations ===

newtype PassRep = PassRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep PassRep
makeWrapped ''PassRep


type    Pass    pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT (PassDataSet (SubPass pass m) pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

-- type ArgSubPass pass m a = Args pass -> SubPass pass m a
-- type ArgPass    pass m   = ArgSubPass pass m ()

-- type DynPassFunc    m   = DynSubPassFunc m ()
-- data DynSubPassFunc m a = DynSubPassFunc { _repr      :: PassRep
--                                  , _rels      :: Description
--                                  , _dynEval   :: m (Either InternalError (m a))
--                                  } deriving (Functor)

-- newtype DynArgs = DynArgs (Prim.Any)

type    DynPassFunc    m   = DynSubPassFunc m ()
newtype DynSubPassFunc m a = DynSubPassFunc (m (Either InternalError (m a))) deriving (Functor)

-- type    DynArgPass    m   = DynArgSubPass m ()
-- newtype DynArgSubPass m a = DynArgSubPass (DynArgs -> DynSubPassFunc m a)

-- instance Show (DynArgSubPass m a) where show _ = "DynArgSubPass"
--
type DynPass    m   = DynSubPass m ()
data DynSubPass m a = DynSubPass { _desc :: !Description
                                 , _func :: !(DynSubPassFunc m a)
                                 } deriving (Show, Functor)

-- data Tagged a = Tagged { _tag  :: !PassRep
--                        , _elm  :: !a
--                        } deriving (Show, Functor, Foldable, Traversable)

data Description = Description { _repr      :: !PassRep
                               , _inputs    :: ![TypeRep]
                               , _outputs   :: ![TypeRep]
                               , _preserves :: ![TypeRep]
                               } deriving (Show)

-- class HasRep a where
--     rep :: a -> PassRep

-- instance HasRep

--FIXME[WD]:
instance Show (DynSubPassFunc m a) where show _ = "DynPassFunc"

type EventKeys      pass = Event <$> Events pass
type PassData       pass = Inputs pass <> Outputs pass <> EventKeys pass -- FIXME (there are duplicates in the list)
type Keys           pass = PassData pass
type PassDataSet  m pass = DataSet m (PassData pass)
-- type PassDataSetM m pass = PassDataSet (PrimState m) pass

type GetPassData m = PassDataSet (GetPassMonad m) (GetPass m)
type family GetPass  m where
    GetPass (SubPass pass m) = pass
    GetPass (t m)            = GetPass m

type family GetPassMonad m where
    GetPassMonad (SubPass pass m) = SubPass pass m
    GetPassMonad (t m)            = GetPassMonad m


makeWrapped ''SubPass
makeLenses  ''DynSubPass
-- makeLenses  ''Tagged
makeLenses  ''Description
makeLenses  ''DynSubPassFunc
makeWrapped ''DynSubPassFunc

-- instance Eq (Desc a) where (==) = (==) `on` view repr ; {-# INLINE (==) #-}

emptyDescription r = Description r def def def

-- === Utils ===

type KnownDescription pass = ( KnownType  (Abstract  pass)
                             , KnownTypes (Inputs    pass)
                             , KnownTypes (Outputs   pass)
                             , KnownTypes (Preserves pass)
                             )

passDescription :: forall pass. KnownDescription pass => Description
passDescription = emptyDescription (typeVal' @(Abstract pass))
                & inputs    .~ typeVals' @(Inputs   pass)
                & outputs   .~ typeVals' @(Outputs  pass)
                & preserves .~ typeVals' @(Outputs  pass)
{-# INLINE passDescription #-}

class                      HasDescription a               where description :: a -> Description
-- instance                   HasDescription (Desc a) where description   = view rels      ; {-# INLINE description #-}
instance KnownDescription p => HasDescription (SubPass p m a) where description _ = passDescription @p ; {-# INLINE description #-}


type Commit m pass = ( Monad m
                     , LookupData pass m (Keys pass)
                     , KnownType  (Abstract  pass)
                     , KnownDescription pass
                     )

class    Functor (p m) => IsPass m p          where compile :: forall a. p m a -> DynSubPass m a
instance Functor m     => IsPass m DynSubPass where compile = id                    ; {-# INLINE compile #-}
instance (PassInit p m, KnownDescription p) => IsPass m (SubPass p)    where compile = DynSubPass (passDescription @p) . DynSubPassFunc . initPass ; {-# INLINE compile #-}


dropResult :: Functor p => p a -> p ()
dropResult = fmap $ const () ; {-# INLINE dropResult #-}

type PassInit pass m = LookupData pass m (Keys pass)

initPass    :: forall pass m a. PassInit pass m => SubPass pass m a  -> m (Either InternalError (m a))
initPass p  = return . fmap (State.evalStateT (unwrap' p)) =<< lookupData @pass ; {-# INLINE initPass    #-}
-- initArgPass :: forall pass m a. PassInit pass m => (Args pass -> SubPass pass m a) -> (Args pass -> m (Either InternalError (m a)))
-- initArgPass = fmap initPass                                                     ; {-# INLINE initArgPass #-}


-- describe :: forall pass m a. (KnownType (Abstract pass), KnownDescription pass, PassInit pass m) => SubPass pass m a -> Desc (DynSubPassFunc m a)
-- describe = Desc (typeVal' @(Abstract pass)) (passDescription @pass) . compile

-- describeA :: forall pass m a. (KnownType (Abstract pass), KnownDescription pass, PassInit pass m) => ArgSubPass pass m a -> Desc (DynArgSubPass m a)
-- describeA f = Desc (typeVal' @(Abstract pass)) (passDescription @pass) $ (\a -> compile (f $ unsafeCoerce a)) -- FIXME[make this unsafecoerce nicer]

eval :: Monad m => DynSubPass m a -> m (Either InternalError a)
eval = join . fmap sequence . run ; {-# INLINE eval #-}

eval' :: (PassInit pass m, KnownDescription pass) => SubPass pass m a -> m (Either InternalError a)
eval' = eval . compile ; {-# INLINE eval' #-}

run :: DynSubPass m a -> m (Either InternalError (m a))
run = unwrap' . view func ; {-# INLINE run #-}



-- === Keys lookup === --

type ReLookupData pass k m ks = (IR.KeyMonad k m (SubPass pass m), LookupData pass m ks, KnownType k)
class    Monad m             => LookupData pass m keys      where lookupData :: m (Either InternalError (DataSet (SubPass pass m) keys))
instance Monad m             => LookupData pass m '[]       where lookupData = return $ return (wrap' List.empty)
instance ReLookupData pass k m ks => LookupData pass m (k ': ks) where lookupData = prepend <<$>> (justErr (MissingData $ typeVal' @k) <$> IR.uncheckedLookupKey)
                                                                                       <<*>> lookupData @pass


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
         , Assert (k `In` (Inputs pass)) (KeyReadError k))
      => Readable k (SubPass pass m) where getKey = view findKey <$> get ; {-# INLINE getKey #-}

-- Writable
instance ( Monad m
         , ContainsKey k (Keys pass)
         , Assert (k `In` (Inputs pass)) (KeyReadError k))
      => Writable k (SubPass pass m) where putKey k = modify_ (findKey .~ k) ; {-# INLINE putKey #-}

-- instance Monad m => Readable k (SubPass pass m) where getKey = undefined
-- instance Monad m => Writable k (SubPass pass m) where putKey = undefined

-- === ContainsKey === --

class                                     ContainsKey k ls        where findKey :: Lens' (DataSet m ls) (Key m k)
instance {-# OVERLAPPING #-}              ContainsKey k (k ': ls) where findKey = head           ; {-# INLINE findKey #-}
instance ContainsKey k ls              => ContainsKey k (l ': ls) where findKey = tail . findKey ; {-# INLINE findKey #-}
instance TypeError (KeyMissingError k) => ContainsKey k '[]       where findKey = impossible     ; {-# INLINE findKey #-}
-- getKey :: m (Key m k)
