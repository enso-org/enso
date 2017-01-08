{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import Prologue
import Prologue.Prim (AnyData)

import           Data.TypeDesc
import           Control.Monad.State       (StateT, evalStateT)
import qualified Control.Monad.State       as State
import           Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.Map                  as Map
import           Data.Map                  (Map)
import qualified Data.Set                  as Set
import           Data.Set                  (Set)
import qualified Data.ManagedVectorMap as Store
import           Data.ManagedVectorMap (ManagedVectorMapRef, ManagedVectorMapRefM, ManagedVectorMap)
import qualified GHC.Prim                    as Prim

import Luna.IR.Layer
import Data.Abstract

class IsIdx t where
    idx :: Iso' t Int
    default idx :: (Wrapped t, Unwrapped t ~ Int) => Lens' t Int
    idx = wrapped' ; {-# INLINE idx #-}



------------------
-- === Elem === --
------------------

-- === Definition === --

data    Element = Element  deriving (Show)
newtype Elem t  = Elem Int deriving (Show, Ord, Eq)
makeWrapped ''Elem

type ElemRep = TypeDescT Element


-- === Instances === --

instance IsIdx (Elem t) where
    idx = wrapped' ; {-# INLINE idx #-}



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
                       -> m (Maybe (Ref' k a m))



-- === RefHandler === --

type family   GetRefHandler (m :: * -> *) :: * -> *
type instance GetRefHandler (MaybeT m)   = GetRefHandler m
type instance GetRefHandler (StateT s m) = GetRefHandler m

type Ref'     k a m = Ref     k a (GetRefHandler m)
type RefData' k a m = RefData k a (GetRefHandler m)



-- MonadRefState
class Monad m => MonadRefState k a m where
    getRef :: m (Ref' k a m)
    putRef :: Ref' k a m -> m ()


type SubMonadRefState k a t m = (MonadRefState k a m, MonadTransInvariants t m, GetRefHandler (t m) ~ GetRefHandler m)
instance {-# OVERLAPPABLE #-} SubMonadRefState k a t m
      => MonadRefState k a (t m) where
    getRef = lift   getRef ; {-# INLINE getRef #-}
    putRef = lift . putRef ; {-# INLINE putRef #-}


getRefData :: forall k a m. MonadRefState k a m => m (RefData' k a m)
getRefData = unwrap' <$> getRef @k @a ; {-# INLINE getRefData #-}

putRefData :: forall k a m. MonadRefState k a m => RefData' k a m -> m ()
putRefData = putRef @k @a . wrap' ; {-# INLINE putRefData #-}


--------------------------
-- === TypeRepGraph === --
--------------------------

-- === Definition === --


newtype TypeRepGraph'  a = TypeRepGraph   (Map ElemRep a) deriving (Show, Default, Functor, Traversable, Foldable)
type    TypeRepGraph     = TypeRepGraph'   ElemStore
type    TypeRepGraphST s = TypeRepGraph'  (ElemStoreST s)
type    TypeRepGraphM  m = TypeRepGraphST (PrimState   m)


-- type LayerSet    s = Store.VectorRef s AnyData
type ElemStore     = ManagedVectorMap      LayerRep AnyData
type ElemStoreST s = ManagedVectorMapRef s LayerRep AnyData
type ElemStoreM  m = ElemStoreST (PrimState m)

makeWrapped ''TypeRepGraph'


-- === Mutability === --

unsafeFreeze :: PrimMonad m => TypeRepGraphM m -> m TypeRepGraph
freeze       :: PrimMonad m => TypeRepGraphM m -> m TypeRepGraph
unsafeFreeze = mapM Store.unsafeFreeze ; {-# INLINE unsafeFreeze #-}
freeze       = mapM Store.freeze       ; {-# INLINE freeze       #-}

unsafeThaw :: PrimMonad m => TypeRepGraph -> m (TypeRepGraphM m)
thaw       :: PrimMonad m => TypeRepGraph -> m (TypeRepGraphM m)
unsafeThaw = mapM Store.unsafeThaw ; {-# INLINE unsafeThaw #-}
thaw       = mapM Store.thaw       ; {-# INLINE thaw       #-}


type family Input a



class Monad m => ElemGraph a g m where
    addElemTo :: g -> m (Elem a, g)
    elemsOf   :: g -> m [Elem a]

class InputTrackingGraph n g m where
    inputs :: Elem n -> g -> m [Elem (Input n)]


-- instance ElemGraph t TypeRepGraph where
--     addElemTo =


-- class MonadGraph m

newtype GraphState g m a = GraphState (StateT g m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow, MonadTrans)
makeWrapped ''GraphState


evalGraphState :: Monad m => GraphState g m a -> g -> m a
evalGraphState = evalStateT . unwrap' ; {-# INLINE evalGraphState #-}


class Monad m => MonadGraphState m where
    getGraph :: m (GetGraph m)
    putGraph :: GetGraph m -> m ()

modifyGraphM :: MonadGraphState m => (GetGraph m -> m (a, GetGraph m)) -> m a
modifyGraphM f = do
    g <- getGraph
    (a, g') <- f g
    putGraph g'
    return a
{-# INLINE modifyGraphM #-}

modifyGraphM_ :: MonadGraphState m => (GetGraph m -> m (GetGraph m)) -> m ()
modifyGraphM_ = modifyGraphM . (fmap.fmap) ((),) ; {-# INLINE modifyGraphM_ #-}

modifyGraph :: MonadGraphState m => (GetGraph m -> (a, GetGraph m)) -> m a
modifyGraph = modifyGraphM . fmap return ; {-# INLINE modifyGraph #-}

modifyGraph_ :: MonadGraphState m => (GetGraph m -> GetGraph m) -> m ()
modifyGraph_ = modifyGraphM_ . fmap return ; {-# INLINE modifyGraph_ #-}

-- modifyGraphM

instance Monad m => MonadGraphState (GraphState g m) where
    getGraph = wrap'   State.get ; {-# INLINE getGraph #-}
    putGraph = wrap' . State.put ; {-# INLINE putGraph #-}
--
instance {-# OVERLAPPABLE #-} (MonadTransInvariants t m, MonadGraphState m, GetGraph (t m) ~ GetGraph m)
      => MonadGraphState (t m) where
    getGraph = lift   getGraph ; {-# INLINE getGraph #-}
    putGraph = lift . putGraph ; {-# INLINE putGraph #-}
--
type family GetGraph m where
    GetGraph (GraphState g m) = g
    GetGraph (t m)            = GetGraph m


instance PrimMonad m => PrimMonad (GraphState g m) where
    type PrimState (GraphState g m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

-- addElem :: Definition t -> m t
-- addElem elDef = do
--     g <- getGraph


-- class Monad m => GraphBuilder a m where
--     addElem :: Definition a -> m (Elem a)


-- instance (PrimMonad m, s ~ PrimState m, KnownType (Abstract t))
--       => GraphBuilder t (GraphState (TypeRepGraphST s) m) where
--     addElem elDef = modifyGraphM $ \g -> do
--         s <- Store.empty
--         i <- Store.reserveIdx s
--         return $ (i ^. from idx, g & wrapped' . at (getTypeDesc @(Abstract t)) ?~ s)
--     {-# INLINE addElem #-}

instance (PrimMonad m, s ~ PrimState m, KnownType (Abstract a))
      => ElemGraph a (TypeRepGraphST s) m where
    addElemTo g = do
        s <- Store.empty
        i <- Store.reserveIdx s
        return $ (i ^. from idx, g & wrapped' . at (getTypeDesc @(Abstract a)) ?~ s)
    {-# INLINE addElemTo #-}


-- instance
--
-- class InputTrackingGraph n g m where
--     inputs :: Elem n -> g -> m [Elem (Input n)]



--
-- instance ElemGraph a g m => GraphBuilder a (GraphState g m) where
--     addElem = modifyGraphM . (lift .: addElemTo) ; {-# INLINE addElem #-}
--
-- instance {-# OVERLAPPABLE #-} (MonadTransInvariants t m, GraphBuilder a m)
--       => GraphBuilder a (t m) where
--     addElem = lift . addElem ; {-# INLINE addElem #-}


addElem :: (MonadGraphState m, ElemGraph a (GetGraph m) m) => m (Elem a)
addElem = modifyGraphM addElemTo








data Inputs

type instance LayerData Inputs t = Set (Elem (Input t))





-- === Node === --

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

type instance Input Inner  = Link' Node
type instance Input Border = Loose


-- type NodeBuilder = GraphBuilder NODE

node :: (MonadGraphState m, ElemGraph Inner (GetGraph m) m) => m Node
node = addElem ; {-# INLINE node #-}

edge :: (MonadGraphState m, ElemGraph (Link a b) (GetGraph m) m) => m (Edge a b)
edge = addElem ; {-# INLINE edge #-}



-- === Edge === --

-- Edge.Type:
data Loose
data Half  a
data Link  a b
type Link' a = Link a a

type LooseEdge     = Elem Loose
type HaldEdge  a   = Elem (Half a)
type Edge      a b = Elem (Link a b)
type Edge'     a   = Edge a a

type instance Definition (Link  a b) = (a,b)
type instance Definition (Half  a)   = a
type instance Definition Loose       = ()


xmain :: IO ()
xmain = do
    flip evalGraphState (def :: TypeRepGraphM IO) $ do
        n1 <- node
        n2 <- node
        n3 <- node
        writeLayer @Inputs mempty n1
        -- e1 <- edge n1 n2
        print n2
        return ()



-- class ManagedVectorMapX layer s el m where
--     readLayerOf  :: s -> Elem el -> m (LayerData layer el)
--     writeLayerOf :: LayerData layer el -> Elem el -> s -> m ()



type family Data key t el

type instance Data Layer l el = LayerData l el


class Store key t el m store where
    writeTo :: store -> Data key t el -> Elem el -> m ()

type Store' key t el m = (Store key t el m (GetStore key m), MonadStore key m)


instance (PrimMonad m, s ~ PrimState m, KnownType layer, KnownType (Abstract el)) => Store Layer layer el m (TypeRepGraphST s) where
    writeTo g d el = do
        let Just s = g ^. wrapped' . at (getTypeDesc @(Abstract el))
        Just v <- Store.readKey (getTypeDesc @layer) s
        Store.unsafeWrite v (el ^. idx) (unsafeCoerce d)


-- !!! Ustawiajac explicite GetStore Layer m ~ ... mozemy pozbyc sie `el` z constraintu i spelnic warunki potrzebne w IR (!)
-- W IR `el` nie moze istniec w constraincie, bo moze to byc np. Expr Draft, moze natomiast istniec `Abstract el`
-- wiec mozemy ustawic `GetStore Layer m ~ IRM m`
writeLayer :: forall layer el m. Store' Layer layer el m => LayerData layer el -> Elem el -> m ()
writeLayer = write @Layer @layer ; {-# INLINE writeLayer #-}

write :: forall key t el m. Store' key t el m => Data key t el -> Elem el -> m ()
write d el = do
    s <- getStore @key
    writeTo @key @t s d el
{-# INLINE write #-}

type family GetStore a (m :: * -> *)

class Monad m => MonadStore a m where
    getStore :: m (GetStore a m)
    putStore :: GetStore a m -> m ()

instance {-# OVERLAPPABLE #-} (MonadStore a m, MonadTransInvariants t m, GetStore a m ~ GetStore a (t m))
      => MonadStore a (t m) where
    getStore = lift $ getStore @a ; {-# INLINE getStore #-}
    putStore = lift . putStore @a ; {-# INLINE putStore #-}


type instance GetStore t (GraphState g m) = g

instance Monad m => MonadStore t (GraphState g m) where
    getStore = getGraph ; {-# INLINE getStore #-}
    putStore = putGraph ; {-# INLINE putStore #-}
