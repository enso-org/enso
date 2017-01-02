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

import           Luna.IR.Internal.IR   (Net, Attr, Refs, Ref(Ref), Reader(..), Writer(..), RefReadError, RefWriteError, GetRefHandler, MonadRef, MonadRefLookup, MonadRefState, liftRefHandler)
import qualified Luna.IR.Internal.IR   as IR
import           Luna.IR.Expr.Layout.Class (Abstract)
import           Type.Maybe                (FromJust)

import Luna.IR.Layer
import Type.List (In)
import qualified GHC.Prim as Prim
import Unsafe.Coerce (unsafeCoerce)
import Data.Event (Event)
import Data.TypeDesc
import System.Log hiding (LookupData, lookupData)
import Data.TList (TList)
import qualified Data.TList as TList
import qualified Data.Map as Map
import           Data.Map (Map)



proxify :: a -> Proxy a
proxify _ = Proxy

-------------------------------

-- === Errors === --

data InternalError = MissingData TypeDesc deriving (Show, Eq)


-- === Properties === --


data PASS = PASS deriving (Show)




--------------------------
-- === Dependencies === --
--------------------------

type family Inputs    t pass :: [*]
type family Outputs   t pass :: [*]
type family Preserves   pass :: [*]
type        Events      pass = Outputs Event pass
type        Elements  t pass = (Inputs t pass <> Outputs t pass)



------------------------------
-- === Pass description === --
------------------------------

-- === Definitions === --

type PassDesc = TypeDescT PASS

data Description = Description { _passRep   :: !PassDesc
                               , _inputs    :: !(Map TypeDesc [TypeDesc])
                               , _outputs   :: !(Map TypeDesc [TypeDesc])
                               , _events    :: ![TypeDesc]
                               , _preserves :: ![TypeDesc]
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
                             , KnownTypes (Inputs  Net   pass)
                             , KnownTypes (Outputs Net   pass)
                             , KnownTypes (Inputs  Layer pass)
                             , KnownTypes (Outputs Layer pass)
                             , KnownTypes (Inputs  Attr  pass)
                             , KnownTypes (Outputs Attr  pass)
                             , KnownTypes (Events        pass)
                             , KnownTypes (Preserves     pass)
                             )

genericDescription :: forall pass. KnownDescription pass => Description
genericDescription = emptyDescription (getTypeDesc @(Abstract pass))
                   & inputs    %~ Map.insert (getTypeDesc @Net)   (getTypeDescs @(Inputs  Net   pass))
                   & outputs   %~ Map.insert (getTypeDesc @Net)   (getTypeDescs @(Outputs Net   pass))
                   & inputs    %~ Map.insert (getTypeDesc @Layer) (getTypeDescs @(Inputs  Layer pass))
                   & outputs   %~ Map.insert (getTypeDesc @Layer) (getTypeDescs @(Outputs Layer pass))
                   & inputs    %~ Map.insert (getTypeDesc @Attr)  (getTypeDescs @(Inputs  Attr  pass))
                   & outputs   %~ Map.insert (getTypeDesc @Attr)  (getTypeDescs @(Outputs Attr  pass))
                   & events    .~ getTypeDescs @(Events    pass)
                   & preserves .~ getTypeDescs @(Preserves pass)
{-# INLINE genericDescription #-}

genericDescription' :: forall pass. KnownDescription pass => Proxy pass -> Description
genericDescription' _ = genericDescription @pass ; {-# INLINE genericDescription' #-}

genericDescriptionP :: KnownDescription pass => pass -> Description
genericDescriptionP = genericDescription' . proxify ; {-# INLINE genericDescriptionP #-}

emptyDescription :: PassDesc -> Description
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
   = RefStore { _netStore   :: TList ( DataStore Net   pass m )
              , _layerStore :: TList ( DataStore Layer pass m )
              , _attrStore  :: TList ( DataStore Attr  pass m )
              , _eventStore :: TList ( DataStore Event pass m )
              }

type DataStore k pass m = Refs k (Elements k pass) m

makeLenses ''RefStore


-- === RefStore preparation === --

type DataLookup m = (MonadRefLookup Net m, MonadRefLookup Layer m, MonadRefLookup Attr m, MonadRefLookup Event m)

lookupRefStore :: forall pass m. DataLookup m
               => Description -> m (Maybe (RefStore' m pass))
lookupRefStore desc = RefStore <<$>> lookupDataStore @Net   @pass @m ((fromJust $ desc ^. inputs . at (getTypeDesc @Net))   <> (fromJust $ desc ^. outputs . at (getTypeDesc @Net)))
                               <<*>> lookupDataStore @Layer @pass @m ((fromJust $ desc ^. inputs . at (getTypeDesc @Layer)) <> (fromJust $ desc ^. outputs . at (getTypeDesc @Layer)))
                               <<*>> lookupDataStore @Attr  @pass @m ((fromJust $ desc ^. inputs . at (getTypeDesc @Attr))  <> (fromJust $ desc ^. outputs . at (getTypeDesc @Attr)))
                               <<*>> lookupDataStore @Event @pass @m (desc ^. events)
    where fromJust (Just a) = a
          lookupDataStore :: forall k pass m. MonadRefLookup k m => [TypeDesc] -> m (Maybe (TList (DataStore k pass (GetRefHandler m))))
          lookupDataStore ts = unsafeCoerce <<$>> unsafeLookupData @k @m ts

          unsafeLookupData :: forall k m. MonadRefLookup k m => [TypeDesc] -> m (Maybe Prim.Any)
          unsafeLookupData []       = return $ return $ unsafeCoerce ()
          unsafeLookupData (t : ts) = unsafeCoerce .: (,) <<$>> IR.uncheckedLookupRef @k t <<*>> unsafeLookupData @k ts


-- === Ref lookup === --

-- FIXME[WD]: make it generic
class ContainsRef pass k a m where findRef :: Lens' (RefStore m pass) (Ref k a m)
instance {-# OVERLAPPING #-} TList.Focus (DataStore Net   pass m) (Ref Net   a m) => ContainsRef pass Net   a m where findRef = netStore   . TList.focus ; {-# INLINE findRef #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore Layer pass m) (Ref Layer a m) => ContainsRef pass Layer a m where findRef = layerStore . TList.focus ; {-# INLINE findRef #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore Attr  pass m) (Ref Attr  a m) => ContainsRef pass Attr  a m where findRef = attrStore  . TList.focus ; {-# INLINE findRef #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore Event pass m) (Ref Event a m) => ContainsRef pass Event a m where findRef = eventStore . TList.focus ; {-# INLINE findRef #-}



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

-- === Definition === --

newtype Proto a = Proto (TypeDesc -> a) deriving (Functor, Applicative, Monad)
makeWrapped ''Proto


-- === Utils === --

specialize :: IsTypeDesc t => Proto a -> t -> a
specialize p = unwrap' p . view typeDesc ; {-# INLINE specialize #-}


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
        fmap (\d -> Template $ \arg -> DynSubPass $ State.evalStateT (unwrap' $ t arg) d) <$> (fromJust desc <$> lookupRefStore @pass desc)
    where fromJust t (Just a) = Right a
          fromJust t Nothing = error $ "!!!" <> show t
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

class Monad m => MonadPass m where
    get :: m (GetPassData m)
    put :: GetPassData m -> m ()

type GetPassData m = RefStore (GetRefHandler m) (GetPass m)
type family GetPass  m where
    GetPass (SubPass pass m) = pass
    GetPass (t m)            = GetPass m


-- === Default instances === --

instance Monad m => MonadPass (SubPass pass m) where
    get = wrap   State.get ; {-# INLINE get #-}
    put = wrap . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadPass m, MonadTrans t, Monad (t m), GetPassData m ~ GetPassData (t m))
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


instance ( Monad m, ContainsRef pass k a (GetRefHandler m) ) -- FIXME[WD]: we can hopefully remove some args from this constraint
      => MonadRefState k a (SubPass pass m) where
    getRef = view findRef <$> get    ; {-# INLINE getRef #-}
    putRef = modify_ . (set findRef) ; {-# INLINE putRef #-}

instance ( Monad m, MonadRefState k a (SubPass pass m)
         , Assert (a `In` (Inputs k pass)) (RefReadError k a)
         ) => Reader k a (SubPass pass m)

instance ( Monad m, MonadRefState k a (SubPass pass m)
         , Assert (a `In` (Outputs k pass)) (RefWriteError k a)
         ) => Writer k a (SubPass pass m)
