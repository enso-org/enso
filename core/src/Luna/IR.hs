module Luna.IR (module Luna.IR, module X) where

import Data.Graph.Component.Node.Destruction as X
import Data.Graph.Data                       as X (destruct, destruct1,
                                                   getAllAllocated)
import Data.Graph.Transform.Substitute       as X
import Luna.IR.Term                          as X
import OCI.Data.Name                         as X (Name, Qualified)
import OCI.IR.Link.Class                     as X (Link, Links, SomeLink,
                                                   Source, Target, source,
                                                   target)
import OCI.IR.Term                           as X

import qualified Luna.IR.Layer as Layer

readLayer  :: ∀ layer t lyt m. Layer.Reader t layer m => t lyt -> m (Layer.Data layer lyt)
writeLayer :: ∀ layer t lyt m. Layer.Writer t layer m => t lyt -> Layer.Data layer lyt -> m ()
readLayer  = Layer.read  @layer
writeLayer = Layer.write @layer
{-# INLINE readLayer  #-}
{-# INLINE writeLayer #-}
