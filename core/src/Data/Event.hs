module Data.Event where

import Prologue

import qualified GHC.Prim as Prim
import qualified Data.Map as Map
import           Data.Map (Map)



--------------------
-- === Events === --
--------------------

-- === Events === --

data    Event    e = Event (Payload e)
type    AnyEvent   = Event Prim.Any
newtype Tag        = Tag [EventRep]   deriving (Show)
newtype EventRep   = EventRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep EventRep

type family Payload (e :: [*])

makeClassy  ''Tag
makeWrapped ''Tag
makeWrapped ''EventRep


-- === Utils === --

class Monad m => Emitter m e where
    emit :: Event e -> m ()


-----------------------
-- === Listeners === --
-----------------------

-- === Functions === --

newtype ListenerRep = ListenerRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep ListenerRep


data ListenerHeader = ListenerHeader { _tag :: Tag
                                     , _rep :: ListenerRep
                                     } deriving (Show)

data Listener f = Listener { _header :: ListenerHeader
                           , _func   :: f
                           } deriving (Show, Functor, Foldable, Traversable)

makePfxClassy ''ListenerHeader
makePfxLenses ''Listener
makeClassy    ''ListenerRep
makeWrapped   ''ListenerRep


-- === Utils === --

eval :: Listener f -> f
eval = _func ; {-# INLINE eval #-}



-- === Instances === --

-- Properties
instance HasListenerRep ListenerHeader where listenerRep = listenerHeader_rep ; {-# INLINE listenerRep #-}
instance HasTag         ListenerHeader where tag         = listenerHeader_tag ; {-# INLINE tag         #-}

instance HasListenerHeader (Listener m) where listenerHeader = listener_header              ; {-# INLINE listenerHeader #-}
instance HasListenerRep    (Listener m) where listenerRep    = listenerHeader . listenerRep ; {-# INLINE listenerRep    #-}
instance HasTag            (Listener m) where tag            = listenerHeader . tag         ; {-# INLINE tag            #-}



-------------------------
-- === ListenerHub === --
-------------------------

-- === Definition === --

data ListenerHub m = ListenerHub { _listeners :: Map ListenerRep (Listener    m)
                                 , _subhubs   :: Map EventRep    (ListenerHub m)
                                 } deriving (Show)

makeLenses ''ListenerHub


-- === Utils === --

attachListener :: Listener m -> ListenerHub m -> ListenerHub m
attachListener l = (space' (l ^. tag) . listeners) %~ Map.insert (l ^. listenerRep) l ; {-# INLINE attachListener #-}

deteachListener :: HasListenerHeader l => l -> ListenerHub m -> ListenerHub m
deteachListener l = space' (h ^. tag) . listeners %~ Map.delete (h ^. listenerRep) where
    h = l ^. listenerHeader
{-# INLINE deteachListener #-}

queryListeners :: Tag -> ListenerHub m -> [Listener m]
queryListeners e = Map.elems . view (space' e . listeners) ; {-# INLINE queryListeners #-}

space :: Tag -> Lens' (ListenerHub m) (Maybe (ListenerHub m))
space = nestedAt . unwrap' ; {-# INLINE space #-}

space' :: Tag -> Lens' (ListenerHub m) (ListenerHub m)
space' = nestedAt' . unwrap' ; {-# INLINE space' #-}


-- === Instances === --

-- Default
instance Default (ListenerHub m) where
    def = ListenerHub def def ; {-# INLINE def #-}

-- Monoid
instance Monoid (ListenerHub m) where
    mempty                                        = def                             ; {-# INLINE mempty  #-}
    mappend (ListenerHub a b) (ListenerHub a' b') = ListenerHub (a <> a') (b <> b') ; {-# INLINE mappend #-}


-- Indexed
type instance IxValue (ListenerHub m) = ListenerHub m
type instance Index   (ListenerHub m) = EventRep

instance At   (ListenerHub m) where at i = subhubs . at i ; {-# INLINE at #-}
instance Ixed (ListenerHub m) where ix i = subhubs . ix i ; {-# INLINE ix #-}
