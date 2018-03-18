{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Class where

import Prologue

import qualified Data.Tag              as Tag
import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import OCI.IR.Conversion (cast)


---------------------
-- === UniTerm === --
---------------------

-- | The implementation of Uni is delayed until we know
--   all possible Term constructors.
type family Uni :: Type -> Type

class IsUni t where
    toUni :: ∀ a. t a -> Uni a



------------------
-- === Term === --
------------------

-- === Definition === --

Component.define "Term"
Tag.family "TermCons"

type SomeTerm = Term ()

type family TagToCons tag = (cons :: Type -> Type) | cons -> tag
type family ConsToTag (cons :: Type -> Type) = tag | tag  -> cons
type TagConsInvariant tag cons =
    ( cons ~ TagToCons tag
    , tag  ~ ConsToTag cons
    )


-- === Creation === --

type Creator t m =
    ( Component.Creator Terms     m
    , Layer.Writer Terms Model    m
    , Layer.DataCons1 Terms Model (TagToCons t)
    )

uncheckedNewM :: Creator tag m
    => (Term any -> m (TagToCons tag layout)) -> m (Term any)
uncheckedNewM cons = do
    ir <- Component.new
    let ir' = cast ir
    term <- cons ir'
    Layer.write @Model ir (Layer.consData1 @Terms @Model term)
    pure ir'
{-# INLINE uncheckedNewM #-}

newM :: ( Creator tag m
        , Layout.Get Model layout ~ tag
        , Layout.AssertEQ Model layout tag
        ) => (Term layout -> m (TagToCons tag layout)) -> m (Term layout)
newM = uncheckedNewM ; {-# INLINE newM #-}

uncheckedNew :: Creator tag m => TagToCons tag layout -> m (Term any)
uncheckedNew = uncheckedNewM . const . pure ; {-# INLINE uncheckedNew #-}

new :: (Creator tag m, Layout.AssertEQ Model layout tag)
    => TagToCons tag layout -> m (Term layout)
new = uncheckedNew ; {-# INLINE new #-}



--------------------
-- === Layers === --
--------------------

-- === Model === --

data Model
type instance Layer.Layout Terms Model layout = layout
type instance Layer.Data   Terms Model = Uni
instance IsUni t => Layer.DataCons1 Terms Model t where
    consData1 = toUni ; {-# INLINE consData1 #-}
