{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term.Construction where

import Prologue

import qualified Data.Graph.Component        as Component
import qualified Data.Graph.Component.Layer  as Layer
import qualified Data.Graph.Component.Layout as Layout
import qualified OCI.IR.Link                 as Link
import qualified OCI.IR.Term.Class           as Term
import qualified OCI.IR.Term.Layer           as Layer

import Data.Graph.Component (Component)
import OCI.IR.Term.Class    (Term, Terms)
import OCI.IR.Term.Layer    (Model)



------------------------------
-- === Top construction === --
------------------------------

-- === DefaultType === --

class DefaultType m where
    defaultType :: m (Term ())


-- === Contexts === --

type UntypedCreator t m =
    ( Component.Creator Terms   m
    , Layer.Writer  Terms Model m
    , Layer.IsCons1 Model (Term.TagToCons t)
    , Layer.IsUnwrapped Term.Uni
    )

type Creator tag m =
    ( UntypedCreator tag m
    , Link.Creator m
    , Layer.Writer Terms Layer.Type m
    , DefaultType m
    )


-- === Construction === --

type LayoutModelCheck tag layout = Layout.Get Model layout ~ tag

uncheckedUntypedNewM :: UntypedCreator tag m
    => (Term any -> m (Term.TagToCons tag layout)) -> m (Term any)
uncheckedUntypedNewM !cons = do
    ir <- Component.construct1' @(Component Terms)
    let !ir' = Layout.unsafeRelayout ir
    !term <- cons ir'
    Layer.write @Model ir $! Layer.cons1 @Model term
    pure ir'
{-# INLINE uncheckedUntypedNewM #-}

untypedNewM :: ( UntypedCreator tag m, LayoutModelCheck tag layout)
     => (Term layout -> m (Term.TagToCons tag layout)) -> m (Term layout)
untypedNewM = uncheckedUntypedNewM ; {-# INLINE untypedNewM #-}

uncheckedUntypedNew :: UntypedCreator tag m
                    => Term.TagToCons tag layout -> m (Term any)
uncheckedUntypedNew = uncheckedUntypedNewM . const . pure ; {-# INLINE uncheckedUntypedNew #-}

untypedNew :: (UntypedCreator tag m, Layout.AssertEQ Model layout tag)
    => Term.TagToCons tag layout -> m (Term layout)
untypedNew = uncheckedUntypedNew ; {-# INLINE untypedNew #-}

uncheckedNewM :: Creator tag m
              => (Term any -> m (Term.TagToCons tag layout)) -> m (Term any)
uncheckedNewM !cons = uncheckedUntypedNewM $ \self -> do
    typeTerm <- defaultType
    typeLink <- Link.new typeTerm self
    Layer.write @Layer.Type self (Layout.unsafeRelayout typeLink)
    cons self
{-# INLINE uncheckedNewM #-}

newM :: (Creator tag m, LayoutModelCheck tag layout)
     => (Term layout -> m (Term.TagToCons tag layout)) -> m (Term layout)
newM = uncheckedNewM ; {-# INLINE newM #-}

uncheckedNew :: Creator tag m => Term.TagToCons tag layout -> m (Term any)
uncheckedNew = uncheckedNewM . const . pure ; {-# INLINE uncheckedNew #-}
