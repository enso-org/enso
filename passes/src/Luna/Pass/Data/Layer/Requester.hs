{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.Requester where

import Prologue

import qualified Data.Construction                     as Data
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Component.Edge             as Edge
import qualified Data.Graph.Component.Edge.Destruction as Edge
import qualified Luna.IR                               as IR
import qualified Luna.IR.Layer                         as Layer

import Data.Graph.Data.Component.Maybe       (MaybeComponent (..))
import Data.Graph.Data.Layer.Class           (Layer)
import Data.Graph.Component.Edge.Class       (type (*-*))
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Component.Edge.Class       (Edges)

data Requester deriving (Generic)
instance Layer Requester where
    type Cons   Requester        = MaybeComponent Edges
    type Layout Requester layout = () *-* layout
    manager = Layer.staticManager

get :: Layer.Reader IR.Term Requester m => IR.Term a -> m (Maybe IR.SomeLink)
get = \t -> do
    link <- unwrap <$> Layer.read @Requester (Layout.relayout t :: IR.SomeTerm)
    return $ Layout.relayout <$> link
{-# INLINE get #-}

set :: ( Layer.Editor IR.Term Requester m
       , Edge.Creator m
       , Edge.Delete  m
       ) => Maybe (IR.Term a) -> IR.Term b -> m ()
set = \src tgt -> do
    old <- get tgt
    traverse_ Edge.delete old
    l <- traverse (flip Edge.new tgt) src
    Layer.write @Requester tgt (wrap $ Layout.relayout <$> l)
{-# INLINE set #-}
