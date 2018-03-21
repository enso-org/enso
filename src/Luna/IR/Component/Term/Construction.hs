{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Construction where

import Prologue

import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import Luna.IR.Component.Term.Class
import Luna.IR.Component.Term.Layer
import OCI.IR.Conversion            (cast)


------------------
-- === Term === --
------------------

-- === Creation === --

type Creator t m =
    ( Component.Creator Terms     m
    , Layer.Writer Terms Model    m
    , Layer.DataCons1 Terms Model (TagToCons t)
    )

type LayoutInference tag layout =
    ( Layout.Get Model layout ~ tag
    , Layout.AssertEQ Model layout tag
    )

uncheckedNewM :: Creator tag m
    => (Term any -> m (TagToCons tag layout)) -> m (Term any)
uncheckedNewM !cons = do
    ir <- Component.new
    let !ir' = cast ir
    !term <- cons ir'
    Layer.write @Model ir $! Layer.consData1 @Terms @Model term
    pure ir'
{-# INLINE uncheckedNewM #-}

newM :: ( Creator tag m, LayoutInference tag layout)
     => (Term layout -> m (TagToCons tag layout)) -> m (Term layout)
newM = uncheckedNewM ; {-# INLINE newM #-}

uncheckedNew :: Creator tag m => TagToCons tag layout -> m (Term any)
uncheckedNew = uncheckedNewM . const . pure ; {-# INLINE uncheckedNew #-}

new :: (Creator tag m, Layout.AssertEQ Model layout tag)
    => TagToCons tag layout -> m (Term layout)
new = uncheckedNew ; {-# INLINE new #-}

