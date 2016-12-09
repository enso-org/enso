{-# LANGUAGE UndecidableInstances #-}

module Data.Event where

import Prologue hiding (tail)

import qualified GHC.Prim   as Prim
import qualified Data.Map   as Map
import           Data.Map   (Map)
import           Data.Reprx (RepOf, HasRep, rep)



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

class                             IsTag a   where toTag :: a -> Tag
instance Typeables (TagPath a) => IsTag a   where toTag _ = Tag $ typeReps' @(TagPath a) ; {-# INLINE toTag #-}
instance {-# OVERLAPPING #-}      IsTag Tag where toTag   = id                           ; {-# INLINE toTag #-}



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
-- === ListenerHub === --
-------------------------

-- === Definition === --

data ListenerHub f = ListenerHub { _listeners :: Map ListenerRep f
                                 , _subhubs   :: Map TagRep (ListenerHub f)
                                 } deriving (Show)

makeLenses ''ListenerHub


-- === Utils === --

attachListener :: Listener -> f -> ListenerHub f -> ListenerHub f
attachListener l f = (space' (l ^. tag) . listeners) %~ Map.insert (l ^. rep) f ; {-# INLINE attachListener #-}

deteachListener :: Listener -> ListenerHub f -> ListenerHub f
deteachListener l = space' (l ^. tag) . listeners %~ Map.delete (l ^. rep) ; {-# INLINE deteachListener #-}

queryListeners :: Tag -> ListenerHub f -> [f]
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
type instance Index   (ListenerHub m) = TagRep

instance At   (ListenerHub m) where at i = subhubs . at i ; {-# INLINE at #-}
instance Ixed (ListenerHub m) where ix i = subhubs . ix i ; {-# INLINE ix #-}
