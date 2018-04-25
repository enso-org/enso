{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue

import qualified OCI.IR.Component    as Component
import qualified OCI.IR.Layer        as Layer
import qualified OCI.IR.Layout       as Layout
import qualified OCI.IR.Ptr.Provider as Ptr

import Data.PtrSet.Mutable          (IsPtr)
import Foreign.Ptr.Utils            (SomePtr)
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

data Source deriving (Generic)
instance Layer  Source where
    type Cons   Source = Term
    type Layout Source layout = Layout.Get Source layout
    manager = Layer.unsafeOnlyDestructorManager

data Target deriving (Generic)
instance Layer  Target where
    type Cons   Target        = Term
    type Layout Target layout = Layout.Get Target layout
    manager = Layer.unsafeOnlyDestructorManager


-- === Helpers === --

source :: Layer.Reader Links Source m
       => Link layout -> m (Term (Layout.Get Source layout))
source = Layer.read @Source ; {-# INLINE source #-}

instance Ptr.Provider1 Link where
    pointersIO1 = pure . pure . convertTo' @SomePtr

----------------------
-- === HasLinks === --
----------------------

class HasLinks a where
    readLinksIO :: a -> IO [SomeLink]

readLinks :: (HasLinks a, MonadIO m) => a -> m [SomeLink]
readLinks = liftIO . readLinksIO
{-# INLINE readLinks #-}

instance HasLinks (Link a) where
    readLinksIO l = pure [coerce l] ; {-# INLINE readLinksIO #-}

instance {-# OVERLAPPABLE #-} HasLinks a where
    readLinksIO _ = pure mempty ; {-# INLINE readLinksIO #-}

target :: Layer.Reader Links Target m
       => Link layout -> m (Term (Layout.Get Target layout))
target = Layer.read @Target ; {-# INLINE target #-}



------------------------
-- === Components === --
------------------------

type Set = Component.Set Links

instance IsPtr a => Ptr.Provider (Set a) where
    pointersIO = Ptr.pointersIO . unwrap

instance Ptr.Provider1 Set where
    pointersIO1 = Ptr.pointersIO1 . unwrap
