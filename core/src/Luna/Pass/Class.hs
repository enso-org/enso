{-# LANGUAGE NoOverloadedStrings     #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Luna.Pass.Class where

import Luna.Prelude hiding (head, tail, elem, repr, Args)
--
-- import           Data.RTuple (List ((:-:)))
-- import qualified Data.RTuple as List

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.State      (StateT)
import           Control.Monad.Primitive

import           Luna.IR.Internal.IR   (NET, LAYER, ATTR, Refs, Ref(Ref), Reader(..), Writer(..), RefReadError, RefWriteError, GetRefHandler, MonadRef, MonadRefLookup, MonadRefState, liftRefHandler)
import qualified Luna.IR.Internal.IR   as IR
import           Luna.IR.Expr.Layout.Class (Abstract)
import           Type.Maybe                (FromJust)

import Luna.IR.Layer hiding (Layers)
import Type.List (In)
import qualified GHC.Prim as Prim
import Unsafe.Coerce (unsafeCoerce)
import Data.Event (EVENT, Event)
import Data.TypeVal
import System.Log hiding (LookupData, lookupData)
import Data.TList (TList)
import qualified Data.TList as TList
import qualified Data.Map as Map
import           Data.Map (Map)





-------------------------------

-- === Errors === --

data InternalError = MissingData TypeRep deriving (Show, Eq)


-- === Properties === --






--------------------------
-- === Dependencies === --
--------------------------

type family Inputs    t pass :: [*]
type family Outputs   t pass :: [*]
type family Preserves   pass :: [*]
type        Events      pass = Outputs EVENT pass
type        Elements  t pass = (Inputs t pass <> Outputs t pass)



------------------------------
-- === Pass description === --
------------------------------

-- === Definitions === --

newtype PassRep = PassRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep PassRep
makeWrapped ''PassRep

data Description = Description { _passRep   :: !PassRep
                               , _inputs    :: !(Map TypeRep [TypeRep])
                               , _outputs   :: !(Map TypeRep [TypeRep])
                               , _events    :: ![TypeRep]
                               , _preserves :: ![TypeRep]
                               } deriving (Show)

data Describbed a = Describbed { _desc    :: !Description
                               , _content :: !a
                               } deriving (Show)

makeLenses  ''Describbed
makeLenses  ''Description


-- === KnownPass === --

class KnownPass pass where
    passDescription :: Description

instance {-# OVERLAPPABLE #-} KnownDescription pass => KnownPass pass where
    passDescription = genericDescription @pass ; {-# INLINE passDescription #-}


-- === Utils === --

type KnownDescription pass = ( KnownType  (Abstract      pass)
                             , KnownTypes (Inputs  NET   pass)
                             , KnownTypes (Outputs NET   pass)
                             , KnownTypes (Inputs  LAYER pass)
                             , KnownTypes (Outputs LAYER pass)
                             , KnownTypes (Inputs  ATTR  pass)
                             , KnownTypes (Outputs ATTR  pass)
                             , KnownTypes (Events        pass)
                             , KnownTypes (Preserves     pass)
                             )

genericDescription :: forall pass. KnownDescription pass => Description
genericDescription = emptyDescription (typeVal' @(Abstract pass))
                   & inputs    %~ Map.insert (typeVal' @NET)   (typeVals' @(Inputs  NET   pass))
                   & outputs   %~ Map.insert (typeVal' @NET)   (typeVals' @(Outputs NET   pass))
                   & inputs    %~ Map.insert (typeVal' @LAYER) (typeVals' @(Inputs  LAYER pass))
                   & outputs   %~ Map.insert (typeVal' @LAYER) (typeVals' @(Outputs LAYER pass))
                   & inputs    %~ Map.insert (typeVal' @ATTR)  (typeVals' @(Inputs  ATTR  pass))
                   & outputs   %~ Map.insert (typeVal' @ATTR)  (typeVals' @(Outputs ATTR  pass))
                   & events    .~ typeVals' @(Events    pass)
                   & preserves .~ typeVals' @(Preserves pass)
{-# INLINE genericDescription #-}

genericDescription' :: forall pass. KnownDescription pass => Proxy pass -> Description
genericDescription' _ = genericDescription @pass ; {-# INLINE genericDescription' #-}

emptyDescription :: PassRep -> Description
emptyDescription r = Description r def def def def ; {-# INLINE emptyDescription #-}

describbed :: forall pass a. KnownPass pass => a -> Describbed a
describbed = Describbed (passDescription @pass) ; {-# INLINE describbed #-}

describbedProxy :: forall pass a. KnownPass pass => Proxy pass -> a -> Describbed a
describbedProxy _ = describbed @pass ; {-# INLINE describbedProxy #-}



----------------------
-- === RefStore === --
----------------------

type RefStore' m pass = RefStore (GetRefHandler m) pass
data RefStore  m pass
   = RefStore { _netStore   :: TList ( DataStore NET   pass m )
              , _layerStore :: TList ( DataStore LAYER pass m )
              , _attrStore  :: TList ( DataStore ATTR  pass m )
              , _eventStore :: TList ( DataStore EVENT pass m )
              }

type DataStore k pass m = Refs k (Elements k pass) m

makeLenses ''RefStore


-- === RefStore preparation === --

type DataLookup m = (MonadRefLookup NET m, MonadRefLookup LAYER m, MonadRefLookup ATTR m, MonadRefLookup EVENT m)

lookupRefStore :: forall pass m. DataLookup m
               => Description -> m (Maybe (RefStore' m pass))
lookupRefStore desc = RefStore <<$>> lookupDataStore @NET   @pass @m ((fromJust $ desc ^. inputs . at (typeVal' @NET))   <> (fromJust $ desc ^. outputs . at (typeVal' @NET)))
                               <<*>> lookupDataStore @LAYER @pass @m ((fromJust $ desc ^. inputs . at (typeVal' @LAYER)) <> (fromJust $ desc ^. outputs . at (typeVal' @LAYER)))
                               <<*>> lookupDataStore @ATTR  @pass @m ((fromJust $ desc ^. inputs . at (typeVal' @ATTR))  <> (fromJust $ desc ^. outputs . at (typeVal' @ATTR)))
                               <<*>> lookupDataStore @EVENT @pass @m (desc ^. events)
    where fromJust (Just a) = a
          lookupDataStore :: forall k pass m. MonadRefLookup k m => [TypeRep] -> m (Maybe (TList (DataStore k pass (GetRefHandler m))))
          lookupDataStore ts = unsafeCoerce <<$>> unsafeLookupData @k @m ts

          unsafeLookupData :: forall k m. MonadRefLookup k m => [TypeRep] -> m (Maybe Prim.Any)
          unsafeLookupData []       = return $ return $ unsafeCoerce ()
          unsafeLookupData (t : ts) = unsafeCoerce .: (,) <<$>> IR.uncheckedLookupRef @k t <<*>> unsafeLookupData @k ts


-- === Ref lookup === --

-- FIXME[WD]: make it generic
class ContainsRef pass k a m where findRef :: Lens' (RefStore m pass) (Ref k a m)
instance {-# OVERLAPPING #-} TList.Focus (DataStore NET   pass m) (Ref NET   a m) => ContainsRef pass NET   a m where findRef = netStore   . TList.focus ; {-# INLINE findRef #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore LAYER pass m) (Ref LAYER a m) => ContainsRef pass LAYER a m where findRef = layerStore . TList.focus ; {-# INLINE findRef #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore ATTR  pass m) (Ref ATTR  a m) => ContainsRef pass ATTR  a m where findRef = attrStore  . TList.focus ; {-# INLINE findRef #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore EVENT pass m) (Ref EVENT a m) => ContainsRef pass EVENT a m where findRef = eventStore . TList.focus ; {-# INLINE findRef #-}



------------------
-- === Pass === --
------------------

-- === Definitions === --

type    Pass    pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT (RefStore' m pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

makeWrapped ''SubPass

-- === Instances === --

instance MonadLogging m => MonadLogging (SubPass pass m)

type instance GetRefHandler (SubPass pass m) = GetRefHandler m
instance (EqPrims m (GetRefHandler m), MonadRef m) => MonadRef (SubPass p m) where
    liftRefHandler = lift . liftRefHandler ; {-# INLINE liftRefHandler #-}



---------------------------
-- === Pass template === --
---------------------------

newtype Template a = Template (Prim.Any -> a) deriving (Functor, Applicative, Monad)
makeWrapped ''Template

-- === Utils === --

template :: (t -> a) -> Template a
template f = Template $ f . unsafeCoerce ; {-# INLINE template #-}

unsafeInstantiate :: t -> Template a -> a
unsafeInstantiate t tmpl = unwrap' tmpl $ unsafeCoerce t ; {-# INLINE unsafeInstantiate #-}

-- === Instances === --

instance Show (Template a) where show _ = "Template" ; {-# INLINE show #-}



----------------------------
-- === Pass prototype === --
----------------------------

newtype Proto a = Proto { specialize :: TypeRep -> a } deriving (Functor, Applicative, Monad)

-- === Instances === --

instance Show (Proto a) where show _ = "Proto" ; {-# INLINE show #-}



----------------------------------
-- === Uninitialized passes === --
----------------------------------

newtype Uninitialized m a = Uninitialized { initialize :: m (Either InternalError a) } deriving (Functor)

instance Show (Uninitialized m a) where show _ = "Uninitialized" ; {-# INLINE show #-}



----------------------------
-- === Dynamic passes === --
----------------------------

-- === Definition === --

type    DynPass    m   = DynSubPass m ()
newtype DynSubPass m a = DynSubPass { runDynPass :: m a } deriving (Show, Functor, Applicative, Monad)


-- === Utils === --

type PassInit pass m = (Logging m, KnownPass pass, DataLookup m)

compileTemplate :: forall pass m a. PassInit pass m
                => Template (SubPass pass m a) -> Uninitialized m (Template (DynSubPass m a))
compileTemplate (Template t) = Uninitialized $ do
    withDebugBy ("Pass [" <> show (desc ^. passRep) <> "]") "Initialzation" $
        fmap (\d -> Template $ \arg -> DynSubPass $ State.evalStateT (unwrap' $ t arg) d) <$> (fromJust <$> lookupRefStore @pass desc)
    where fromJust (Just a) = Right a
          desc = passDescription @pass
{-# INLINE compileTemplate #-}

compile :: forall pass m a. PassInit pass m
        => SubPass pass m a -> Uninitialized m (DynSubPass m a)
compile t = Uninitialized $ do
    withDebugBy ("Pass [" <> show (desc ^. passRep) <> "]") "Initialzation" $
        fmap (\d -> DynSubPass $ State.evalStateT (unwrap' t) d) <$> (fromJust <$> lookupRefStore @pass desc)
    where fromJust (Just a) = Right a
          desc = passDescription @pass
{-# INLINE compile #-}


eval :: Monad m => Uninitialized m (DynSubPass m a) -> m (Either InternalError a)
eval = join . fmap (sequence . fmap runDynPass) . initialize ; {-# INLINE eval #-}

eval' :: PassInit pass m => SubPass pass m a -> m (Either InternalError a)
eval' = eval . compile ; {-# INLINE eval' #-}



----------------------------
-- === ElemScope pass === --
----------------------------

-- === Definition === --

-- | ElemScope pass denotes passes which work per graph element, like layer constructors.
--   Every ElemScope pass can be compiled to dynamic form.

data ElemScope pass t = ElemScope deriving (Show)
class KnownElemPass pass where
    elemPassDescription :: forall t. KnownType (Abstract t) => ElemScope pass t -> Description

-- === Intances === --

type instance Abstract (ElemScope pass t) = ElemScope (Abstract pass) (Abstract t)

instance (KnownElemPass pass, KnownType (Abstract t)) => KnownPass (ElemScope pass t) where
    passDescription = elemPassDescription (ElemScope :: ElemScope pass t) ; {-# INLINE passDescription #-}



-----------------------
-- === MonadPass === --
-----------------------

-- === Definitions === --

-- | MonadPassManager_Boot is a hack allowing cross-module recursive dependencies with MonadPassManager
type family MonadPassManager_Boot (m :: * -> *) :: Constraint

class (Monad m, MonadPassManager_Boot m) => MonadPass m where
    get :: m (GetPassData m)
    put :: GetPassData m -> m ()

type GetPassData m = RefStore (GetRefHandler m) (GetPass m)
type family GetPass  m where
    GetPass (SubPass pass m) = pass
    GetPass (t m)            = GetPass m


-- === Default instances === --

instance (Monad m, MonadPassManager_Boot (SubPass pass m)) => MonadPass (SubPass pass m) where
    get = wrap   State.get ; {-# INLINE get #-}
    put = wrap . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadPass m, MonadTrans t, Monad (t m), GetPassData m ~ GetPassData (t m), MonadPassManager_Boot (t m))
      => MonadPass (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}


-- === Modification === --

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


instance ( Monad m
         , ContainsRef pass k a (GetRefHandler m) -- FIXME[WD]: we can hopefully remove some args from this constraint
         , MonadPassManager_Boot (SubPass pass m))
      => MonadRefState k a (SubPass pass m) where
    getRef = view findRef <$> get    ; {-# INLINE getRef #-}
    putRef = modify_ . (set findRef) ; {-# INLINE putRef #-}


instance ( Monad m, MonadPassManager_Boot (SubPass pass m)
         , MonadRefState k a (SubPass pass m)
         , Assert (a `In` (Inputs k pass)) (RefReadError k a)
         ) => Reader k a (SubPass pass m)

instance ( Monad m, MonadPassManager_Boot (SubPass pass m)
         , MonadRefState k a (SubPass pass m)
         , Assert (a `In` (Outputs k pass)) (RefWriteError k a)
         ) => Writer k a (SubPass pass m)
