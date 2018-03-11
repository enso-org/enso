{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term where

import Prologue

import qualified Data.Tag              as Tag
import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import OCI.IR.Component  (Component)
import OCI.IR.Conversion (cast)



------------------
-- === Term === --
------------------

-- === Definition === --

Component.componentInstance "Term"
Tag.family "TermCons"

type SomeTerm = Term ()

type family TagToCons tag = (cons :: Type -> Type) | cons -> tag
type family ConsToTag (cons :: Type -> Type) = tag | tag  -> cons
type TagConsInvariant tag cons =
    ( cons ~ TagToCons tag
    , tag  ~ ConsToTag cons
    )


-- === Uni === --

type family Uni :: Type -> Type

class IsUni t where
    toUni :: ∀ a. t a -> Uni a


-- === Basic layers === --

data Model
type instance Layer.Layout Terms Model layout = layout
type instance Layer.Data   Terms Model = Uni
instance IsUni t => Layer.DataCons1 Terms Model t where
    consData1 = toUni ; {-# INLINE consData1 #-}


-- === Creation === --

type Creator t m =
    ( Component.Creator Terms     m
    , Layer.Writer Terms Model    m
    , Layer.DataCons1 Terms Model (TagToCons t)
    )

uncheckedNew :: forall tag layout m any. Creator tag m
             => TagToCons tag layout -> m (Term any)
uncheckedNew term = do
    ir <- Component.new
    Layer.write @Model ir (Layer.consData1 @Terms @Model term)
    pure $ cast ir
{-# INLINE uncheckedNew #-}

new :: forall tag layout m. Creator tag m
    => TagToCons tag layout -> m (Term (Layout.Set Model tag layout))
new = uncheckedNew ; {-# INLINE new #-}
