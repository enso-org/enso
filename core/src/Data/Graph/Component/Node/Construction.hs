{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Node.Construction where

import Prologue

import qualified Data.Graph.Component.Edge       as Edge
import qualified Data.Graph.Component.Node.Class as Node
import qualified Data.Graph.Component.Node.Layer as Layer
import qualified Data.Graph.Data                 as Component
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout

import Data.Graph.Component.Node.Class (Node, Nodes)
import Data.Graph.Component.Node.Layer (Model)
import Data.Graph.Data                 (Component)



------------------------------
-- === Top construction === --
------------------------------

-- === DefaultType === --

class DefaultType m where
    defaultType :: m (Node ())


-- === Contexts === --

type UntypedCreator t m =
    ( Component.Creator Nodes   m
    , Layer.Writer  Node  Model m
    , Layer.IsCons1 Model (Node.TagToCons t)
    , Layer.IsUnwrapped Node.Uni
    )

type Creator tag m =
    ( UntypedCreator tag m
    , Edge.Creator m
    , Layer.Writer Node Layer.Type m
    , DefaultType m
    )

-- FIXME: Clean up all the Creator...
type CreatorX m =
    ( Component.Creator Nodes   m
    , Layer.Writer  Node  Model m
    , Edge.Creator m
    , Layer.Writer Node Layer.Type m
    , DefaultType m
    )


-- === Construction === --

type LayoutModelCheck tag layout = Layout.Get Model layout ~ tag

uncheckedUntypedNewM :: UntypedCreator tag m
    => (Node any -> m (Node.TagToCons tag layout)) -> m (Node any)
uncheckedUntypedNewM !cons = do
    ir <- Component.construct1' @(Component Nodes)
    let !ir' = Layout.unsafeRelayout ir
    !term <- cons ir'
    Layer.write @Model ir $! Layer.cons1 @Model term
    pure ir'
{-# INLINE uncheckedUntypedNewM #-}

untypedNewM :: ( UntypedCreator tag m, LayoutModelCheck tag layout)
     => (Node layout -> m (Node.TagToCons tag layout)) -> m (Node layout)
untypedNewM = uncheckedUntypedNewM ; {-# INLINE untypedNewM #-}

uncheckedUntypedNew :: UntypedCreator tag m
                    => Node.TagToCons tag layout -> m (Node any)
uncheckedUntypedNew = uncheckedUntypedNewM . const . pure ; {-# INLINE uncheckedUntypedNew #-}

untypedNew :: (UntypedCreator tag m, Layout.AssertEQ Model layout tag)
    => Node.TagToCons tag layout -> m (Node layout)
untypedNew = uncheckedUntypedNew ; {-# INLINE untypedNew #-}

uncheckedNewM :: Creator tag m
              => (Node any -> m (Node.TagToCons tag layout)) -> m (Node any)
uncheckedNewM !cons = uncheckedUntypedNewM $ \self -> do
    typeNode <- defaultType
    typeEdge <- Edge.new typeNode self
    Layer.write @Layer.Type self (Layout.unsafeRelayout typeEdge)
    cons self
{-# INLINE uncheckedNewM #-}

newM :: (Creator tag m, LayoutModelCheck tag layout)
     => (Node layout -> m (Node.TagToCons tag layout)) -> m (Node layout)
newM = uncheckedNewM ; {-# INLINE newM #-}

uncheckedNew :: Creator tag m => Node.TagToCons tag layout -> m (Node any)
uncheckedNew = uncheckedNewM . const . pure ; {-# INLINE uncheckedNew #-}
