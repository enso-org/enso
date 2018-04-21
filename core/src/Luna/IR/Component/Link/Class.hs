{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component as Component
import qualified OCI.IR.Layer     as Layer
import qualified OCI.IR.Layout    as Layout

import Data.Generics.Traversable    (GTraversable, gfoldlM)
import Luna.IR.Component.Term.Class (Term)
import OCI.IR.Layer                 (Layer)
import OCI.IR.Layout                ((:=), Layout)



------------------
-- === Link === --
------------------

-- === Definition === ---

Component.define "Link"
type SomeLink = Link ()
type src *-* tgt = Layout '[Source := src, Target := tgt]



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Source
instance Layer  Source where
    type Cons   Source = Term
    type Layout Source layout = Layout.Get Source layout
    manager = Layer.unsafeOnlyDestructorManager

data Target
instance Layer  Target where
    type Cons   Target        = Term
    type Layout Target layout = Layout.Get Target layout
    manager = Layer.unsafeOnlyDestructorManager

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT


-- === Helpers === --

source :: Layer.Reader Links Source m
       => Link layout -> m (Term (Layout.Get Source layout))
source = Layer.read @Source ; {-# INLINE source #-}

target :: Layer.Reader Links Target m
       => Link layout -> m (Term (Layout.Get Target layout))
target = Layer.read @Target ; {-# INLINE target #-}



----------------------
-- === Provider === --
----------------------

-- === Definition === --

class Provider  a where linksIO  ::        a     -> IO [SomeLink]
class Provider1 a where linksIO1 :: ∀ t1. (a t1) -> IO [SomeLink]


-- === API === --

links  :: (Provider  a, MonadIO m) => a    -> m [SomeLink]
links1 :: (Provider1 a, MonadIO m) => a t1 -> m [SomeLink]
links  = liftIO . linksIO  ; {-# INLINE links  #-}
links1 = liftIO . linksIO1 ; {-# INLINE links1 #-}

glinks :: (GTraversable Provider a, MonadIO m) => a -> m [SomeLink]
glinks = gfoldlM @Provider (\acc a -> (acc <>) <$> links a) mempty ; {-# INLINE glinks #-}

gglinks :: (GTraversable (GTraversable Provider) a, MonadIO m) => a -> m [SomeLink]
gglinks = gfoldlM @(GTraversable Provider) (\acc a -> (acc <>) <$> glinks a) mempty ; {-# INLINE gglinks #-}


-- === Default instances === --

instance Provider1 Link where
    linksIO1 = pure . pure . Layout.relayout ; {-# INLINE linksIO1 #-}

instance {-# OVERLAPPABLE #-} Provider1 a => Provider (a t1) where
    linksIO = linksIO1 ; {-# INLINE linksIO #-}



------------------------
-- === Components === --
------------------------

type Set = Component.Set Links
