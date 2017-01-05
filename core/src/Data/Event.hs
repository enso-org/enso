{-# LANGUAGE UndecidableInstances #-}

module Data.Event (module Data.Event, module X) where

import Prologue hiding (tail)

import qualified GHC.Prim      as Prim
import qualified Data.Map      as Map
import           Data.Map      (Map)
import           Data.Reprx    (RepOf, HasRep, rep)
import           Data.TypeDesc
import           Data.Path     as X
-- WIP:
import           Luna.IR.Expr.Layout.Class (Abstract)



data Event

-----------------
-- === Tag === --
-----------------

newtype Tag     = Tag [TagDesc] deriving (Eq, Ord)
type    TagDesc = TypeDescT Tag

makeClassy  ''Tag
makeWrapped ''Tag

instance Show Tag where
    show (Tag ts) = intercalate " // " $ show <$> ts


-- === Construction === --

type family TagPath a where
    TagPath (a // as) = a ': TagPath as
    TagPath a         = '[a]

type KnownTag tag = KnownTypes (TagPath tag)
fromPath :: forall tag. KnownTag tag => Tag
fromPath = Tag $ getTypeDescs @(TagPath tag) ; {-# INLINE fromPath #-}

-- FIXME[WD]
fromPathDyn :: TypeDesc -> Tag
fromPathDyn t = if con == sepType then let [l,r] = args in wrapped' %~ (wrap' l :) $ fromPathDyn r
                                  else Tag [wrap' t]
    where sepType     = getTypeDesc @TypeSep ^. tyCon
          (con, args) = splitTyConApp t
{-# INLINE fromPathDyn #-}


data Tagged a = Tagged { _eventTag :: !Tag
                       , _content  :: !a
                       } deriving (Show, Functor)
makeLenses ''Tagged





--------------------
-- === Events === --
--------------------

-- === Events === --

data Payload e = Payload (PayloadData e)
type AnyEvent  = Payload Prim.Any

type family PayloadData e

makeWrapped ''Payload


-- === Emitter === --

class Monad m => Emitter a m where
    emit :: forall e. a ~ Abstract e => Payload e -> m ()

type family Emitters as m :: Constraint where
    Emitters '[]       m = ()
    Emitters (a ': as) m = (Emitter a m, Emitters as m)


-----------------------
-- === Listeners === --
-----------------------

-- === Functions === --

data Listener l = Listener { _tag :: Tag
                           , _rep :: l
                           } deriving (Show)

type    Listener'   = Listener ListenerRep
newtype ListenerRep = ListenerRep TypeDesc deriving (Show, Eq, Ord)
instance IsTypeDesc ListenerRep

makePfxLenses ''Listener
makeWrapped   ''ListenerRep


-- === Instances === --

-- Properties
type instance RepOf  (Listener l) = l
instance      HasRep (Listener l) where rep = listener_rep ; {-# INLINE rep #-}
instance      HasTag (Listener l) where tag = listener_tag ; {-# INLINE tag #-}



-------------------------
-- === EventHub === --
-------------------------

-- === Definition === --

data EventHub l f = EventHub { _listeners :: Map l f
                             , _subhubs   :: Map TagDesc (EventHub l f)
                             } deriving (Show)

makeLenses ''EventHub


-- === Utils === --

attachListener :: Ord l => Listener l -> f -> EventHub l f -> EventHub l f
attachListener l f = (space' (l ^. tag) . listeners) %~ Map.insert (l ^. rep) f ; {-# INLINE attachListener #-}

deteachListener :: Ord l => Listener l -> EventHub l f -> EventHub l f
deteachListener l = space' (l ^. tag) . listeners %~ Map.delete (l ^. rep) ; {-# INLINE deteachListener #-}

queryListeners :: Ord l => Tag -> EventHub l f -> [f]
queryListeners e = Map.elems . view (space' e . listeners) ; {-# INLINE queryListeners #-}

space :: Ord l => Tag -> Lens' (EventHub l f) (Maybe (EventHub l f))
space = nestedAt . unwrap' ; {-# INLINE space #-}

space' :: Ord l => Tag -> Lens' (EventHub l f) (EventHub l f)
space' = nestedAt' . unwrap' ; {-# INLINE space' #-}


-- === Instances === --

-- Default
instance Default (EventHub l f) where
    def = EventHub def def ; {-# INLINE def #-}

-- Monoid
instance Ord l => Monoid (EventHub l f) where
    mempty                                  = def                          ; {-# INLINE mempty  #-}
    mappend (EventHub a b) (EventHub a' b') = EventHub (a <> a') (b <> b') ; {-# INLINE mappend #-}


-- Indexed
type instance IxValue (EventHub l f) = EventHub l f
type instance Index   (EventHub l f) = TagDesc

instance At   (EventHub l f) where at i = subhubs . at i ; {-# INLINE at #-}
instance Ixed (EventHub l f) where ix i = subhubs . ix i ; {-# INLINE ix #-}
