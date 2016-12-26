{-# LANGUAGE NoOverloadedStrings  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Luna.IR.Internal.IR   (NET, LAYER, ATTR, Keys, Key(Key), Reader(..), Writer(..), KeyReadError, KeyMissingError, GetPassMonad)
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


-- data PRESERVE

-- type family Desc t pass :: [*]
-- type family Networks  pass :: [*]
-- type family Layers    pass :: [*]
-- type family Attribs   pass :: [*]

type family Inputs  t pass :: [*]
type family Outputs t pass :: [*]
-- type family Events    pass :: [*]
type family Preserves pass :: [*]

type Events pass = Outputs EVENT pass

type Elements t pass = (Inputs t pass <> Outputs t pass)


type DataStore k pass m = Keys k (Elements k pass) m

---------------------
-- === DataSet === --
---------------------

data DataSet m pass
   = DataSet { _netStore   :: TList ( DataStore NET   pass m )
             , _layerStore :: TList ( DataStore LAYER pass m )
             , _attrStore  :: TList ( DataStore ATTR  pass m )
             , _eventStore :: TList ( DataStore EVENT pass m )
             }
-- type    DataSetM m     = DataSet (PrimState m)
makeLenses ''DataSet

-- prepend :: Key m k -> DataSet m ks -> DataSet m (k ': ks)
-- prepend k = wrapped %~ TList.prepend k ; {-# INLINE prepend #-}

-- | FIXME[WD]: tail cannot be constructed as wrapped . TList.tail . Why?
-- tail :: Lens' (DataSet m (k ': ks)) (DataSet m ks)
-- tail = lens (wrapped %~ (view TList.tail)) $ flip (\lst -> wrapped %~ (TList.tail .~ unwrap' lst)) ; {-# INLINE tail #-}
--
-- head :: Lens' (DataSet m (k ': ks)) (Key m k)
-- head = wrapped . TList.head ; {-# INLINE head #-}



-------------------------------

-- === Errors === --

data InternalError = MissingData TypeRep deriving (Show, Eq)


-- === Properties === --



-- === Template === --

newtype Template a = Template (Prim.Any -> a) deriving (Functor, Applicative, Monad)
makeWrapped ''Template

template :: (t -> a) -> Template a
template f = Template $ f . unsafeCoerce ; {-# INLINE template #-}

unsafeInstantiate :: t -> Template a -> a
unsafeInstantiate t tmpl = unwrap' tmpl $ unsafeCoerce t ; {-# INLINE unsafeInstantiate #-}

instance Show (Template a) where show _ = "Template" ; {-# INLINE show #-}


-- === Proto === --

newtype Proto a = Proto { specialize :: TypeRep -> a } deriving (Functor)

instance Show (Proto a) where show _ = "Proto" ; {-# INLINE show #-}


-- === Data declarations ===

newtype PassRep = PassRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep PassRep
makeWrapped ''PassRep


type    Pass    pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT (DataSet m pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

-- type ArgSubPass pass m a = Args pass -> SubPass pass m a
-- type ArgPass    pass m   = ArgSubPass pass m ()

-- type DynPass2    m   = DynSubPass2 m ()
-- data DynSubPass2 m a = DynSubPass2 { _repr      :: PassRep
--                                  , _rels      :: Description
--                                  , _dynEval   :: m (Either InternalError (m a))
--                                  } deriving (Functor)

-- newtype DynArgs = DynArgs (Prim.Any)

type    DynPass2    m   = DynSubPass2 m ()
newtype DynSubPass2 m a = DynSubPass2 (m (Either InternalError (m a))) deriving (Functor)

type    DynPass3    m   = DynSubPass3 m ()
newtype DynSubPass3 m a = DynSubPass3 { runDynPass :: m a } deriving (Show, Functor, Applicative, Monad)

newtype Initializer m a = Initializer { runInitializer :: m (Either InternalError a) } deriving (Functor)

instance Show (Initializer m a) where show _ = "Initializer" ; {-# INLINE show #-}

type DynSubPassTemplate m a = Template (DynSubPass2 m a)
type DynPassTemplate    m   = Template (DynPass2    m)

type SubPassTemplate pass m a = Template (SubPass pass m a)
type PassTemplate    pass m   = Template (Pass    pass m)


-- type    DynArgPass    m   = DynArgSubPass m ()
-- newtype DynArgSubPass m a = DynArgSubPass (DynArgs -> DynSubPass2 m a)

-- instance Show (DynArgSubPass m a) where show _ = "DynArgSubPass"
--
type DynPass    m   = DynSubPass m ()
data DynSubPass m a = DynSubPass { _desc :: !Description
                                 , _func :: !(DynSubPass2 m a)
                                 } deriving (Show, Functor)

-- data Tagged a = Tagged { _tag  :: !PassRep
--                        , _elm  :: !a
--                        } deriving (Show, Functor, Foldable, Traversable)

data Description = Description { _passRep   :: !PassRep
                               , _inputs    :: !(Map TypeRep [TypeRep])
                               , _outputs   :: !(Map TypeRep [TypeRep])
                               , _events    :: ![TypeRep]
                               , _preserves :: ![TypeRep]
                               } deriving (Show)

data Describbed a = Describbed { _desc2   :: !Description
                               , _content :: !a
                               } deriving (Show)

-- class HasRep a where
--     rep :: a -> PassRep

-- instance HasRep

--FIXME[WD]:
instance Show (DynSubPass2 m a) where show _ = "DynPass2"

-- type EventKeys      pass = Event <$> Events pass
-- type PassData       pass = Inputs pass <> Outputs pass <> EventKeys pass -- FIXME (there are duplicates in the list)
-- type Keys           pass = PassData pass

type GetPassData m = DataSet (GetPassMonad m) (GetPass m)
type family GetPass  m where
    GetPass (SubPass pass m) = pass
    GetPass (t m)            = GetPass m



-- FIXME[WD]: after refactoring keys here, merge def with instance
type instance GetPassMonad m = GetPassMonad' m
type family GetPassMonad' m where
    GetPassMonad' (SubPass pass m) = m
    GetPassMonad' (t m)            = GetPassMonad' m

-- FIXME[WD]: merge with real MonadPass
instance IR.IRMonad m => IR.MonadPass (SubPass pass m) where
    liftPass = lift ; {-# INLINE liftPass #-}


instance MonadLogging m => MonadLogging (SubPass pass m)

makeWrapped ''SubPass
makeLenses  ''DynSubPass
makeLenses  ''Describbed
-- makeLenses  ''Tagged
makeLenses  ''Description
makeLenses  ''DynSubPass2
makeWrapped ''DynSubPass2

-- instance Eq (Desc a) where (==) = (==) `on` view repr ; {-# INLINE (==) #-}

emptyDescription r = Description r def def def def

-- === Utils ===

data ElemScope pass t = ElemScope

type instance Abstract (ElemScope pass t) = ElemScope (Abstract pass) (Abstract t)

class KnownElemPass pass where
    elemPassDescription :: forall t. KnownType (Abstract t) => ElemScope pass t -> Description

class KnownPass pass where
    passDescription :: Description

instance {-# OVERLAPPABLE #-} KnownDescription pass => KnownPass pass where
    passDescription = genericDescription @pass ; {-# INLINE passDescription #-}

instance (KnownElemPass pass, KnownType (Abstract t)) => KnownPass (ElemScope pass t) where
    passDescription = elemPassDescription (ElemScope :: ElemScope pass t)

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

describbed :: forall pass a. KnownPass pass => a -> Describbed a
describbed = Describbed (passDescription @pass) ; {-# INLINE describbed #-}

describbedProxy :: forall pass a. KnownPass pass => Proxy pass -> a -> Describbed a
describbedProxy _ = describbed @pass ; {-# INLINE describbedProxy #-}


type DataLookup m = (IR.KeyMonad NET m, IR.KeyMonad LAYER m, IR.KeyMonad ATTR m, IR.KeyMonad EVENT m)

lookupDataSet :: forall pass m. DataLookup m
              => Description -> m (Maybe (DataSet m pass))
lookupDataSet desc = DataSet <<$>> lookupDataStore @NET   @pass @m ((fromJust $ desc ^. inputs . at (typeVal' @NET))   <> (fromJust $ desc ^. outputs . at (typeVal' @NET)))
                             <<*>> lookupDataStore @LAYER @pass @m ((fromJust $ desc ^. inputs . at (typeVal' @LAYER)) <> (fromJust $ desc ^. outputs . at (typeVal' @LAYER)))
                             <<*>> lookupDataStore @ATTR  @pass @m ((fromJust $ desc ^. inputs . at (typeVal' @ATTR))  <> (fromJust $ desc ^. outputs . at (typeVal' @ATTR)))
                             <<*>> lookupDataStore @EVENT @pass @m (desc ^. events)
    where fromJust (Just a) = a
          lookupDataStore :: forall k pass m. IR.KeyMonad k m => [TypeRep] -> m (Maybe (TList (DataStore k pass m)))
          lookupDataStore ts = unsafeCoerce <<$>> unsafeLookupData @k @m ts

          unsafeLookupData :: forall k m. IR.KeyMonad k m => [TypeRep] -> m (Maybe Prim.Any)
          unsafeLookupData []       = return $ return $ unsafeCoerce ()
          unsafeLookupData (t : ts) = unsafeCoerce .: (,) <<$>> IR.uncheckedLookupKey @k t <<*>> unsafeLookupData @k ts


--
-- data DataSet m pass
--    = DataSet { _netStore   :: TList ( DataStore NET   pass m )
--              , _layerStore :: TList ( DataStore LAYER pass m )
--              , _attrStore  :: TList ( DataStore ATTR  pass m )
--              , _eventStore :: TList ( DataStore EVENT pass m )
--              }


-- class                      HasDescription a               where description :: a -> Description
-- -- instance                   HasDescription (Desc a) where description   = view rels      ; {-# INLINE description #-}
-- instance KnownPass p => HasDescription (SubPass p m a) where description _ = passDescription @p ; {-# INLINE description #-}


-- type Commit m pass = ( Monad m
--                     --  , LookupData pass m (Keys pass)
--                      , KnownType  (Abstract  pass)
--                      , KnownPass pass
--                      )

-- class    Functor (p m)                      => Copilable m p           where compile :: forall a. p m a -> DynSubPass m a
-- instance Functor m                          => Copilable m DynSubPass  where compile = id                                                          ; {-# INLINE compile #-}
-- instance (PassInit p m, KnownPass p) => Copilable m (SubPass p) where compile = DynSubPass (passDescription @p) . DynSubPass2 . initPass ; {-# INLINE compile #-}
--
-- class    Functor (p m)  => Copilable2 m p           where compile2 :: forall a. p m a -> DynSubPass2 m a
-- -- instance Functor m      => Copilable2 m DynSubPass  where compile2 = id                        ; {-# INLINE compile2 #-}
-- instance (PassInit p m) => Copilable2 m (SubPass p) where compile2 = DynSubPass2 . initPass ; {-# INLINE compile2 #-}
--
--
-- dropResult :: Functor p => p a -> p ()
-- dropResult = fmap $ const () ; {-# INLINE dropResult #-}
--
type PassInit pass m = (Logging m, KnownPass pass, DataLookup m)  -- LookupData pass m (Keys pass), KnownType (Abstract pass), Logging m)
--
-- initPass :: forall pass m a. PassInit pass m => SubPass pass m a -> m (Either InternalError (m a))
-- initPass p = do
--     withDebugBy ("Pass [" <> show (typeVal' @(Abstract pass) :: TypeRep) <> "]") "Initialzation" $
--         fmap (\d -> withDebugBy ("Pass [" <> show (typeVal' @(Abstract pass) :: TypeRep) <> "]") "Running" $ State.evalStateT (unwrap' p) d) <$> lookupData @pass
-- {-# INLINE initPass #-}

-- FIXME[WD]: merge initialize and initialize'
initialize :: forall pass m a. PassInit pass m
           => Template (SubPass pass m a) -> Initializer m (Template (DynSubPass3 m a))
initialize (Template t) = Initializer $ do
    withDebugBy ("Pass [" <> show (desc ^. passRep) <> "]") "Initialzation" $
        fmap (\d -> Template $ \arg -> DynSubPass3 $ State.evalStateT (unwrap' $ t arg) d) <$> (fromJust <$> lookupDataSet @pass desc)
    where fromJust (Just a) = Right a
          desc = passDescription @pass

initialize' :: forall pass m a. PassInit pass m
            => SubPass pass m a -> Initializer m (DynSubPass3 m a)
initialize' t = Initializer $ do
    withDebugBy ("Pass [" <> show (desc ^. passRep) <> "]") "Initialzation" $
        fmap (\d -> DynSubPass3 $ State.evalStateT (unwrap' t) d) <$> (fromJust <$> lookupDataSet @pass desc)
    where fromJust (Just a) = Right a
          desc = passDescription @pass



-- State.evalStateT (unwrap' p)
-- initArgPass :: forall pass m a. PassInit pass m => (Args pass -> SubPass pass m a) -> (Args pass -> m (Either InternalError (m a)))
-- initArgPass = fmap initPass                                                     ; {-# INLINE initArgPass #-}


-- describe :: forall pass m a. (KnownType (Abstract pass), KnownDescription pass, PassInit pass m) => SubPass pass m a -> Desc (DynSubPass2 m a)
-- describe = Desc (typeVal' @(Abstract pass)) (genericDescription @pass) . compile

-- describeA :: forall pass m a. (KnownType (Abstract pass), KnownDescription pass, PassInit pass m) => ArgSubPass pass m a -> Desc (DynArgSubPass m a)
-- describeA f = Desc (typeVal' @(Abstract pass)) (genericDescription @pass) $ (\a -> compile (f $ unsafeCoerce a)) -- FIXME[make this unsafecoerce nicer]



eval :: Monad m => Initializer m (DynSubPass3 m a) -> m (Either InternalError a)
eval = join . fmap (sequence . fmap runDynPass) . runInitializer ; {-# INLINE eval #-}

eval' :: PassInit pass m => SubPass pass m a -> m (Either InternalError a)
eval' = eval . initialize' ; {-# INLINE eval' #-}

run :: DynSubPass m a -> m (Either InternalError (m a))
run = unwrap' . view func ; {-# INLINE run #-}



-- === Keys lookup === --

-- type ReLookupData pass k m ks = (IR.KeyMonad k m (SubPass pass m), LookupData pass m ks, KnownType k)
class    Monad m                  => LookupData pass m      where lookupData :: m (Either InternalError (DataSet (SubPass pass m) pass))
-- instance Monad m                  => LookupData pass m '[]       where lookupData = undefined --return $ return (wrap' TList.empty)
-- instance ReLookupData pass k m ks => LookupData pass m (k ': ks) where lookupData = undefined -- prepend <<$>> (justErr (MissingData $ typeVal' @k) <$> IR.uncheckedLookupKey)
                                                                                        --    <<*>> lookupData @pass




-- buildStore :: Store ks
-- class Monad m => LookupData2 m where
--     lookupData2 :: forall pass. Typeable (Abstract pass) => m (Either InternalError (DataSet (SubPass pass m) (Keys pass)))

-- instance Monad m => LookupData2 m where
--     lookupData2 = lookupData3
--
-- class Monad m => LookupData3 m keys where
--     lookupData3 :: forall pass. Typeable (Abstract pass) => m (Either InternalError (DataSet (SubPass pass m) keys))
--

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


-- FIXME[WD]: add asserts
-- Reader
instance ( Monad m
         , ContainsKey pass k a m
         , Assert (a `In` (Inputs k pass)) (KeyReadError k a))
      => Reader k a (SubPass pass m) where getKey = view findKey <$> get ; {-# INLINE getKey #-}

-- Writer
instance ( Monad m
         , ContainsKey pass k a m
         , Assert (a `In` (Inputs k pass)) (KeyReadError k a))
      => Writer k a (SubPass pass m) where putKey k = modify_ (findKey .~ k) ; {-# INLINE putKey #-}


-- === ContainsKey === --

-- FIXME[WD]: make it generic
class ContainsKey pass k a m where findKey :: Lens' (DataSet m pass) (Key k a m)
instance {-# OVERLAPPING #-} TList.Focus (DataStore NET   pass m) (Key NET   a m) => ContainsKey pass NET   a m where findKey = netStore   . TList.focus ; {-# INLINE findKey #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore LAYER pass m) (Key LAYER a m) => ContainsKey pass LAYER a m where findKey = layerStore . TList.focus ; {-# INLINE findKey #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore ATTR  pass m) (Key ATTR  a m) => ContainsKey pass ATTR  a m where findKey = attrStore  . TList.focus ; {-# INLINE findKey #-}
instance {-# OVERLAPPING #-} TList.Focus (DataStore EVENT pass m) (Key EVENT a m) => ContainsKey pass EVENT a m where findKey = eventStore . TList.focus ; {-# INLINE findKey #-}

-- instance {-# OVERLAPPING #-} ()
--       => ContainsKey pass EVENT a m where findKey = netStore . TList.focus --  netStore . TList.focus ; {-# INLINE findKey #-}
-- instance ContainsKey k ls              => ContainsKey pass k (l ': ls) where findKey = tail . findKey ; {-# INLINE findKey #-}
-- instance TypeError (KeyMissingError k) => ContainsKey pass k '[]       where findKey = impossible     ; {-# INLINE findKey #-}
-- getKey :: m (Key m k)
--
