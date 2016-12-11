{-# LANGUAGE UndecidableInstances #-}

module Data.Event where

import Prologue hiding (tail)

import qualified GHC.Prim     as Prim
import qualified Data.Map     as Map
import           Data.Map     (Map)
import           Data.Reprx   (RepOf, HasRep, rep)
import           Data.TypeVal


-----------------
-- === Tag === --
-----------------

newtype Tag    = Tag    [TagRep] deriving (Show, Eq, Ord)
newtype TagRep = TagRep TypeRep  deriving (Show, Eq, Ord)
instance IsTypeRep TagRep

makeClassy  ''Tag
makeWrapped ''Tag
makeWrapped ''TagRep


-- === Construction === --

type family TagPath a where
    TagPath (a // as) = a ': TagPath as
    TagPath a         = '[a]

type FromPath path = Typeables (TagPath path)
fromPath :: forall path. FromPath path => Tag
fromPath = Tag $ typeReps' @(TagPath path) ; {-# INLINE fromPath #-}

data a // as = a :// as
infixr 5 //
(//) :: a -> as -> a // as
a // as = a :// as ; {-# INLINE (//) #-}

tail :: a // as -> as
tail (_ :// as) = as ; {-# INLINE tail #-}


-- === IsTag === --

-- instance Typeables (TagPath a) => IsTag a   where toTag _ = Tag $ typeReps' @(TagPath a) ; {-# INLINE toTag #-}
class                                       IsTag a        where toTag :: a -> Tag
instance {-# OVERLAPPABLE #-} IsTagSeg t => IsTag t        where toTag t         = Tag [toTagSeg t]                     ; {-# INLINE toTag #-}
instance (IsTagSeg t, IsTag p)           => IsTag (t // p) where toTag (t :// p) = toTag p & wrapped' %~ (toTagSeg t :) ; {-# INLINE toTag #-}
instance                                    IsTag Tag      where toTag           = id                                   ; {-# INLINE toTag #-}

class                        IsTagSeg a       where toTagSeg :: a -> TagRep
instance KnownType a      => IsTagSeg a       where toTagSeg _ = typeVal' @a ; {-# INLINE toTagSeg #-}
instance {-# OVERLAPPING #-} IsTagSeg TypeRep where toTagSeg   = wrap'       ; {-# INLINE toTagSeg #-} -- FIXME[WD]: in order not to accidentally pass here some Rep like structure (like LayerRep), there should be some data RepBase aroundd


--------------------
-- === Events === --
--------------------

-- === Events === --

data Event e  = Event (Payload e)
type AnyEvent = Event Prim.Any

type family Payload e

makeWrapped ''Event


-- === Emitter === --

class Monad m => Emitter m e where
    emit :: e -> Payload e -> m ()



-----------------------
-- === Listeners === --
-----------------------

-- === Functions === --

data Listener = Listener { _tag :: Tag
                         , _rep :: ListenerRep
                         } deriving (Show)

newtype ListenerRep = ListenerRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep ListenerRep

makePfxClassy ''Listener
makeWrapped   ''ListenerRep


-- === Instances === --

-- Properties
type instance RepOf Listener = ListenerRep
instance HasRep Listener where rep = listener_rep ; {-# INLINE rep #-}
instance HasTag Listener where tag = listener_tag ; {-# INLINE tag #-}



-------------------------
-- === EventHub === --
-------------------------

-- === Definition === --

data EventHub f = EventHub { _listeners :: Map ListenerRep f
                           , _subhubs   :: Map TagRep (EventHub f)
                           } deriving (Show)

makeLenses ''EventHub


-- === Utils === --

attachListener :: Listener -> f -> EventHub f -> EventHub f
attachListener l f = (space' (l ^. tag) . listeners) %~ Map.insert (l ^. rep) f ; {-# INLINE attachListener #-}

deteachListener :: Listener -> EventHub f -> EventHub f
deteachListener l = space' (l ^. tag) . listeners %~ Map.delete (l ^. rep) ; {-# INLINE deteachListener #-}

queryListeners :: Tag -> EventHub f -> [f]
queryListeners e = Map.elems . view (space' e . listeners) ; {-# INLINE queryListeners #-}

space :: Tag -> Lens' (EventHub m) (Maybe (EventHub m))
space = nestedAt . unwrap' ; {-# INLINE space #-}

space' :: Tag -> Lens' (EventHub m) (EventHub m)
space' = nestedAt' . unwrap' ; {-# INLINE space' #-}


-- === Instances === --

-- Default
instance Default (EventHub m) where
    def = EventHub def def ; {-# INLINE def #-}

-- Monoid
instance Monoid (EventHub m) where
    mempty                                        = def                             ; {-# INLINE mempty  #-}
    mappend (EventHub a b) (EventHub a' b') = EventHub (a <> a') (b <> b') ; {-# INLINE mappend #-}


-- Indexed
type instance IxValue (EventHub m) = EventHub m
type instance Index   (EventHub m) = TagRep

instance At   (EventHub m) where at i = subhubs . at i ; {-# INLINE at #-}
instance Ixed (EventHub m) where ix i = subhubs . ix i ; {-# INLINE ix #-}
