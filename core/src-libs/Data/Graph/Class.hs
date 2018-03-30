{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import Prologue_old      hiding (Data)
import Prologue.Prim (AnyData)

import           Data.TypeDesc
-- import           Control.Monad.State       (StateT, evalStateT)
import qualified Control.Monad.State       as OldState
import           Control.Monad.State.Dependent
import qualified Control.Monad.Except      as Except
import           Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.Map                  as Map
import           Data.Map                  (Map)
import qualified Data.Set                  as Set
import           Data.Set                  (Set)
import qualified Data.ManagedVectorMap as VectorMap
import           Data.ManagedVectorMap (ManagedVectorMapRef, ManagedVectorMapRefM, ManagedVectorMap)
import           Data.Container.Mutable

import OCI.IR.Layer.Class
import Data.Abstract


-------------------
-- === Store === --
-------------------

-- === Definition === --

type family Store s (m :: * -> *)

class Monad m => MonadStore s m where
    getStore :: m (Store s m)
    putStore :: Store s m -> m ()


-- === Modification === --

modifyStore   :: forall s m a . MonadStore s m => (Store s m ->   (a, Store s m)) -> m a
modifyStore_  :: forall s m   . MonadStore s m => (Store s m ->       Store s m)  -> m ()
modifyStoreM_ :: forall s m   . MonadStore s m => (Store s m -> m    (Store s m)) -> m ()
modifyStoreM  :: forall s m a . MonadStore s m => (Store s m -> m (a, Store s m)) -> m a

modifyStore   = modifyStoreM  @s . fmap return
modifyStore_  = modifyStoreM_ @s . fmap return
modifyStoreM_ = modifyStoreM  @s . (fmap.fmap) ((),)
modifyStoreM f = do
    g <- getStore @s
    (a, g') <- f g
    putStore @s g'
    return a


-- === Default instances === --

instance {-# OVERLAPPABLE #-} (MonadStore s m, MonadTransInvariants t m, Store s m ~ Store s (t m))
      => MonadStore s (t m) where
    getStore = lift $ getStore @s
    putStore = lift . putStore @s



-----------------------
-- === StoreData === --
-----------------------

type family Data key t el



------------------
-- === Elem === --
------------------

-- === Definition === --

data    Element = Element  deriving (Show)
newtype Elem t  = Elem Int deriving (Show, Ord, Eq)
makeWrapped ''Elem

type ElemRep = TypeDescT Element


-- === GraphElem === --

class GraphElem t where
    idx :: Iso' t Int
    default idx :: (Wrapped t, Unwrapped t ~ Int) => Lens' t Int
    idx = wrapped'

instance GraphElem (Elem t) where
    idx = wrapped'


-- === ElemStore === --

class ElemStore key t el m store where
    writeToElemStore :: store -> Elem el -> Data key t el -> m ()

type MonadElemStore key t el m = (ElemStore key t el m (Store key m), MonadStore key m)



----------------------
-- === Some TFs === --
----------------------

data Net = Net deriving (Show)
type family Input a

data Inputs

type instance LayerData Inputs t = Set (Elem (Input t))



-------------------
-- === Graph === --
-------------------

-- === Definition === --

class Monad m => ElemGraph a g m where
    addElemTo :: g -> m (Elem a, g)
    elemsOf   :: g -> m [Elem a]

class InputTrackingGraph n g m where
    inputs :: Elem n -> g -> m [Elem (Input n)]


-- === Modification === --

type MonadElemGraph a m = (MonadStore Net m, ElemGraph a (Store Net m) m)

addElem :: MonadElemGraph a m => m (Elem a)
addElem = modifyStoreM @Net addElemTo



------------------------
-- === GraphState === --
------------------------

-- === Definition == --

newtype GraphState g m a = GraphState (OldState.StateT g m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow, MonadTrans)
makeWrapped ''GraphState

-- === Running === --

evalGraphState :: Monad m => GraphState g m a -> g -> m a
evalGraphState = OldState.evalStateT . unwrap'


-- === MonadGraphState === --

class Monad m => MonadGraphState m where
    getGraph :: m (GetGraph m)
    putGraph :: GetGraph m -> m ()

type family GetGraph m where
    GetGraph (GraphState g m) = g
    GetGraph (t m)            = GetGraph m


-- === Modification === --

modifyGraphM  :: MonadGraphState m => (GetGraph m -> m (a, GetGraph m)) -> m a
modifyGraphM_ :: MonadGraphState m => (GetGraph m -> m    (GetGraph m)) -> m ()
modifyGraph   :: MonadGraphState m => (GetGraph m ->   (a, GetGraph m)) -> m a
modifyGraph_  :: MonadGraphState m => (GetGraph m ->       GetGraph m)  -> m ()
modifyGraphM_  = modifyGraphM  . (fmap.fmap) ((),)
modifyGraph    = modifyGraphM  . fmap return
modifyGraph_   = modifyGraphM_ . fmap return
modifyGraphM f = do
    g <- getGraph
    (a, g') <- f g
    putGraph g'
    return a


-- === Default Instances === --

instance Monad m => MonadGraphState (GraphState g m) where
    getGraph = wrap'   OldState.get
    putGraph = wrap' . OldState.put


-- === Instances === ---

-- Primitive
instance PrimMonad m => PrimMonad (GraphState g m) where
    type PrimState (GraphState g m) = PrimState m
    primitive = lift . primitive

-- Store
type instance Store t (GraphState g m) = g
instance Monad m => MonadStore t (GraphState g m) where
    getStore = getGraph
    putStore = putGraph



----------------
----------------
----------------


------------------
-- === Node === --
------------------

data NODE

-- Node.Type
data Inner
data Border
data Free

type Node       = Elem Inner
type BorderNode = Elem Border
type FreeNode   = Elem Free

type instance Definition Inner  = ()
type instance Definition Border = ()
type instance Definition Free   = ()

type instance Abstract Inner  = NODE
type instance Abstract Border = NODE
type instance Abstract Free   = NODE

type instance Input Inner  = EdgeType_Arc' Node
type instance Input Border = EdgeType_Loose


node :: MonadElemGraph Inner m => m Node
node = addElem



------------------
-- === Edge === --
------------------

-- Edge.Type:
data EdgeType_Loose
data EdgeType_Half  a
data EdgeType_Arc   a b
type EdgeType_Arc'  a = EdgeType_Arc a a

type LooseEdge     = Elem  EdgeType_Loose
type HaldEdge  a   = Elem (EdgeType_Half a)
type Edge      a b = Elem (EdgeType_Arc a b)
type Edge'     a   = Edge a a

type instance Definition (EdgeType_Arc  a b) = (a,b)
type instance Definition (EdgeType_Half  a)  = a
type instance Definition  EdgeType_Loose     = ()


edge :: MonadElemGraph (EdgeType_Arc a b) m => m (Edge a b)
edge = addElem




--------------------
-- === Layers === --
--------------------

type instance Data Layer l el = LayerData l el

writeLayer :: forall layer el m. MonadElemStore Layer layer el m => Elem el -> LayerData layer el -> m ()
writeLayer = writeElemProp @Layer @layer

writeElemProp :: forall key t el m. MonadElemStore key t el m => Elem el -> Data key t el -> m ()
writeElemProp el d = do
    s <- getStore @key
    writeToElemStore @key @t s el d



-- FIXME[WD]: Refs are not needed in this module. In fact we should consider removing them at all.
------------------
-- === Refs === --
------------------

--- === Definition === --

newtype Ref k a m = Ref (RefData k a m)
type family RefData k a (m :: * -> *)
makeWrapped ''Ref


-- === Utils === --

type family Refs k as m where
    Refs k '[]       m = '[]
    Refs k (a ': as) m = Ref k a m ': Refs k as m


-- === Ref lookup === --

class Monad m => MonadRefLookup k m where
    uncheckedLookupRef :: forall a. TypeDesc {- the type of `a` -}
                       -> m (Ref' k a m)

instance {-# OVERLAPPABLE #-} (MonadRefLookup k m, MonadTrans t, Monad (t m), GetRefHandler m ~ GetRefHandler (t m))
      => MonadRefLookup k (t m) where
    uncheckedLookupRef = lift . uncheckedLookupRef


-- === Ref store === --

class Monad m => MonadRefStore k m where
    uncheckedStoreRef  :: forall a. TypeDesc -> Ref' k a m -> m ()

instance {-# OVERLAPPABLE #-} (MonadRefStore k m, MonadTrans t, Monad (t m), GetRefHandler m ~ GetRefHandler (t m))
      => MonadRefStore k (t m) where
    uncheckedStoreRef = lift .: uncheckedStoreRef


-- === RefHandler === --

type family   GetRefHandler (m :: * -> *) :: * -> *
type instance GetRefHandler (MaybeT m)            = GetRefHandler m
type instance GetRefHandler (Except.ExceptT e m)  = GetRefHandler m
type instance GetRefHandler (StateT s m)          = GetRefHandler m
type instance GetRefHandler (OldState.StateT s m) = GetRefHandler m

type Ref'     k a m = Ref     k a (GetRefHandler m)
type RefData' k a m = RefData k a (GetRefHandler m)



-- MonadRefState
class Monad m => MonadRefState k a m where
    getRef :: m (Ref' k a m)
    putRef :: Ref' k a m -> m ()


type SubMonadRefState k a t m = (MonadRefState k a m, MonadTransInvariants t m, GetRefHandler (t m) ~ GetRefHandler m)
instance {-# OVERLAPPABLE #-} SubMonadRefState k a t m
      => MonadRefState k a (t m) where
    getRef = lift   getRef
    putRef = lift . putRef


getRefData :: forall k a m. MonadRefState k a m => m (RefData' k a m)
getRefData = unwrap' <$> getRef @k @a

putRefData :: forall k a m. MonadRefState k a m => RefData' k a m -> m ()
putRefData = putRef @k @a . wrap'





----------------------------------------------------
------------------ Graph backends ------------------
----------------------------------------------------



--------------------------
-- === TypeRepGraph === --
--------------------------

-- === Definition === --

newtype ElemRepMapGraph a = ElemRepMapGraph (Map ElemRep a) deriving (Show, Eq, Default, Functor, Traversable, Foldable)
type    TypeRepGraph      = ElemRepMapGraph TypeRepVectorMap
type    TypeRepGraphST  s = ElemRepMapGraph (TypeRepVectorMapST s)
type    TypeRepGraphM   m = TypeRepGraphST  (PrimState m)

type TypeRepVectorMap     = ManagedVectorMap      LayerRep AnyData
type TypeRepVectorMapST s = ManagedVectorMapRef s LayerRep AnyData
type TypeRepVectorMapM  m = TypeRepVectorMapST (PrimState m)

makeWrapped ''ElemRepMapGraph


-- === Mutability === --

type instance Mutable s TypeRepGraph = TypeRepGraphST s
instance PrimMonad m => Freezable       m TypeRepGraph where freeze       = mapM VectorMap.freeze
instance PrimMonad m => UnsafeFreezable m TypeRepGraph where unsafeFreeze = mapM VectorMap.unsafeFreeze
instance PrimMonad m => Thawable        m TypeRepGraph where thaw         = mapM VectorMap.thaw
instance PrimMonad m => UnsafeThawable  m TypeRepGraph where unsafeThaw   = mapM VectorMap.unsafeThaw


-- === Instances === --

type instance Index   (ElemRepMapGraph a) = ElemRep
type instance IxValue (ElemRepMapGraph a) = a
instance Ixed (ElemRepMapGraph a) where ix i = wrapped . ix i
instance At   (ElemRepMapGraph a) where at i = wrapped . at i


instance (PrimMonad m, s ~ PrimState m, KnownType (Abstract a))
      => ElemGraph a (TypeRepGraphST s) m where
    addElemTo g = do
        s <- VectorMap.empty
        i <- VectorMap.reserveIdx s
        return $ (i ^. from idx, g & wrapped' . at (getTypeDesc @(Abstract a)) ?~ s)

instance (PrimMonad m, s ~ PrimState m, KnownType layer, KnownType (Abstract el))
      => ElemStore Layer layer el m (TypeRepGraphST s) where
    writeToElemStore g el d = do
        let Just s = g ^. wrapped' . at (getTypeDesc @(Abstract el))
        Just v <- VectorMap.readKey (getTypeDesc @layer) s
        VectorMap.unsafeWrite v (el ^. idx) (unsafeCoerce d)



----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


xmain :: IO ()
xmain = do
    flip evalGraphState (def :: TypeRepGraphM IO) $ do
        n1 <- node
        n2 <- node
        n3 <- node
        print n2
        -- writeLayer @Inputs n1 mempty
        -- e1 <- edge n1 n2
        return ()
