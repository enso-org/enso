module OCI.IR.Link.Construction where

import Prologue

import qualified Data.Graph.Component        as Component
import qualified Data.Graph.Component.Layer  as Layer
import qualified Data.Graph.Component.Layout as Layout
import qualified Data.Set.Mutable.Class      as Set

import OCI.IR.Link.Class
import OCI.IR.Term.Class (Term, Terms)
import OCI.IR.Term.Layer (Users)



------------------
-- === Link === --
------------------

-- === Construction === --

type Creator m =
    ( Component.Creator Links m
    , Layer.Writer Links Source m
    , Layer.Writer Links Target m
    , Layer.Editor Terms Users  m
    )

new :: Creator m => Term src -> Term tgt -> m (Link (src *-* tgt))
new src tgt = do
    link    <- Component.new
    userMap <- Layer.read @Users src
    Set.insert userMap (Layout.unsafeRelayout link)
    Layer.write @Source link src
    Layer.write @Target link tgt
    pure link
{-# INLINE new #-}
