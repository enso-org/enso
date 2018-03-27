module Luna.IR.Component.Link.Construction where

import Prologue
import Type.Data.Ord

import qualified Data.IntSet.Cpp       as IntSet
import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import Luna.IR.Component.Link.Class
import Luna.IR.Component.Term.Class (Term, Terms)
import Luna.IR.Component.Term.Layer (Users)
import OCI.IR.Layout                ((:=), Layout)



------------------
-- === Link === --
------------------

-- === Construction === --

type Creator m =
    ( Component.Creator Links m
    , Layer.Writer Links Source m
    , Layer.Writer Links Target m
    -- , Layer.Editor Terms Users  m
    )

new :: Creator m => Term src -> Term tgt -> m (Link (src *-* tgt))
new src tgt = do
    link    <- Component.new
    -- userMap <- Layer.read @Users src
    -- Layer.write @Users  src $ IntSet.insert userMap
    Layer.write @Source link src
    Layer.write @Target link tgt
    pure link
{-# INLINE new #-}

