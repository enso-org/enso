module Luna.IR.Group where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import Luna.IR.Term.Class (Term)
import OCI.IR.Conversion  (generalize)
import OCI.IR.Layout      ((:=), Layout)



------------------
-- === Group === --
------------------

-- === Definition === ---

Component.define "Group"
type SomeGroup = Group ()


-- === Construction === --

-- type Creator m =
--     ( Component.Creator Groups m
--     , Layer.Writer Groups Source m
--     , Layer.Writer Groups Target m
--     )

-- new :: Creator m => [Term a] -> m (Group a)
-- new src tgt = do
--     ir <- Component.new
--     Layer.write @Source ir src
--     Layer.write @Target ir tgt
--     pure $ ir
-- {-# INLINE new #-}



--------------------
-- === Layers === --
--------------------

data Children

-- type instance Layer.Layout Groups Source layout = Layout.Get Source layout
-- type instance Layer.Data   Groups Source        = Term
