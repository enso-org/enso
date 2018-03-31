{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Construction where

import Prologue

import qualified Data.Tag                          as Tag
import qualified Foreign.Storable.Deriving         as Storable
import qualified Foreign.Storable1.Deriving        as Storable1
import qualified Luna.IR.Component.Link            as Link
import qualified Luna.IR.Component.Link.TH         as Link
import qualified Luna.IR.Component.Term.Definition as Term
import qualified Luna.IR.Component.Term.Discovery  as Discovery
import qualified Luna.IR.Component.Term.Layer      as Layer
import qualified OCI.IR.Component                  as Component
import qualified OCI.IR.Layer.Internal             as Layer
import qualified OCI.IR.Layout                     as Layout

import Luna.IR.Component.Term.Class
import Luna.IR.Component.Term.Layer
import Luna.IR.Component.Term.Layout ()


----------------------
-- === Top term === --
----------------------

-- | A very special kind of term. The 'Top' term is unresolved, it is just
--   a term of any value. It belongs to 'Core', however it's defined here
--   because of a special implementation of 'Type' layer initialization.

Term.define [d| data Top a = Top |]

type UntypedCreator t m =
    ( Component.Creator Terms   m
    , Layer.Writer  Terms Model m
    , Layer.IsCons1 Terms Model (TagToCons t)
    )

type Creator tag m =
    ( UntypedCreator tag m
    , UntypedCreator Top m
    , Link.Creator m
    , Layer.Writer Terms Layer.Type m
    )

top :: Creator Top m => m (Term Top)
top = uncheckedUntypedNewM $ \self -> do
    typeLink <- Link.new self self
    Layer.write @Layer.Type self (Layout.relayout typeLink)
    pure Top
{-# INLINE top #-}



------------------
-- === Term === --
------------------

-- === Construction === --

type LayoutInference tag layout =
    ( Layout.Get Model layout ~ tag
    , Layout.AssertEQ Model layout tag
    )

uncheckedUntypedNewM :: UntypedCreator tag m
    => (Term any -> m (TagToCons tag layout)) -> m (Term any)
uncheckedUntypedNewM !cons = do
    ir <- Component.new
    let !ir' = Layout.unsafeRelayout ir
    !term <- cons ir'
    Layer.write @Model ir $! Layer.cons1 @Terms @Model term
    pure ir'
{-# INLINE uncheckedUntypedNewM #-}

untypedNewM :: ( UntypedCreator tag m, LayoutInference tag layout)
     => (Term layout -> m (TagToCons tag layout)) -> m (Term layout)
untypedNewM = uncheckedUntypedNewM ; {-# INLINE untypedNewM #-}

uncheckedUntypedNew :: UntypedCreator tag m => TagToCons tag layout -> m (Term any)
uncheckedUntypedNew = uncheckedUntypedNewM . const . pure ; {-# INLINE uncheckedUntypedNew #-}

untypedNew :: (UntypedCreator tag m, Layout.AssertEQ Model layout tag)
    => TagToCons tag layout -> m (Term layout)
untypedNew = uncheckedUntypedNew ; {-# INLINE untypedNew #-}

uncheckedNewM :: Creator tag m
              => (Term any -> m (TagToCons tag layout)) -> m (Term any)
uncheckedNewM !cons = uncheckedUntypedNewM $ \self -> do
    typeTerm <- top
    typeLink <- Link.new typeTerm self
    Layer.write @Layer.Type self (Layout.unsafeRelayout typeLink)
    cons self
{-# INLINE uncheckedNewM #-}

newM :: (Creator tag m, LayoutInference tag layout)
     => (Term layout -> m (TagToCons tag layout)) -> m (Term layout)
newM = uncheckedNewM ; {-# INLINE newM #-}

uncheckedNew :: Creator tag m => TagToCons tag layout -> m (Term any)
uncheckedNew = uncheckedNewM . const . pure ; {-# INLINE uncheckedNew #-}
