{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.Interface where

import Prologue

import qualified OCI.IR.Layer        as Layer
import qualified OCI.Pass.Attr       as Attr
import qualified OCI.Pass.Definition as Pass


-----------------------
-- === Interface === --
-----------------------

-- | Pass 'Interface' is used to automatically define constraints for passes
--   similar to provided pass description. For example:
--
-- > data Analysis
-- > type instance Spec Analysis t = Spec_Analysis t
-- > type family   Spec_Analysis t where
-- >     Spec_Analysis (In Elems) = '[Terms, Links]
-- >     Spec_Analysis (In Terms) = '[Model, Type]
-- >     Spec_Analysis (In Links) = '[Source, Target]
-- >     Spec_Analysis (Out a)    = Spec_Analysis (In a)
-- >
-- > test :: Pass.Interface Analysis m => m ()
-- > test = ...
--

type family Interface pass m :: Constraint where
    Interface pass m = Interface__ pass m

type Interface__ pass m =
    ( MapCompIface Layer.Reader m pass (Pass.Ins  pass Pass.Elems)
    , MapCompIface Layer.Writer m pass (Pass.Outs pass Pass.Elems)
    , MapAttrIface Attr.Getter  m      (Pass.Ins  pass Pass.Attrs)
    , MapAttrIface Attr.Setter  m      (Pass.Outs pass Pass.Attrs)
    )


-- === Components === --

type family MapCompIface ctx m pass comps :: Constraint where
    MapCompIface ctx m pass '[]       = ()
    MapCompIface ctx m pass (c ': cs) = ( CompIface  ctx m c (Pass.Ins pass c)
                                        , MapCompIface ctx m pass cs
                                        )


type family CompIface ctx (m :: Type -> Type) comp layers :: Constraint where
    CompIface ctx m comp '[]       = ()
    CompIface ctx m comp (l ': ls) = (ctx comp l m, CompIface ctx m comp ls)


-- === Attributes === --

type family MapAttrIface ctx (m :: Type -> Type) attrs :: Constraint where
    MapAttrIface ctx m '[]       = ()
    MapAttrIface ctx m (a ': as) = (ctx a m, MapAttrIface ctx m as)
