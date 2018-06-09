{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.Definition.Interface where

import Prologue

import qualified Data.Graph.Component.Node.Construction as Term
import qualified Data.Graph.Data.Layer.Class            as Layer
import qualified OCI.Pass.Definition.Declaration        as Pass
import qualified OCI.Pass.State.Attr                    as Attr

import Data.Graph.Data (Component)


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
--   It is also possible to list multiple interfaces using the same API, like:
--
-- > test :: Pass.Interface '[Analysis, Simplification] m => m ()
-- > test = ...

type family   Interface (pass :: k) (m :: Type -> Type) :: Constraint
type instance Interface passes m = Interfaces  passes m
type instance Interface pass   m = Interface__ pass   m

type family Interfaces passes m :: Constraint where
    Interfaces '[]       m = ()
    Interfaces (i ': is) m = (Interface__ i m, Interfaces is m)


type Interface__ pass m =
    ( Term.CreatorX m
    , MapCompLayers Layer.Reader m pass Pass.In  (Pass.Vars pass Pass.Elems)
    , MapCompLayers Layer.Writer m pass Pass.Out (Pass.Vars pass Pass.Elems)
    , MapCtx Attr.Getter  m                (Pass.Ins  pass Pass.Attrs)
    , MapCtx Attr.Setter  m                (Pass.Outs pass Pass.Attrs)
    )


-- === Components === --

type family MapCompLayers ctx m pass s comps :: Constraint where
    MapCompLayers ctx m pass s '[]       = ()
    MapCompLayers ctx m pass s (c ': cs) = ( CompIface ctx m c (Pass.Resolve s pass c)
                                           , MapCompLayers ctx m pass s cs
                                           )


type family CompIface ctx (m :: Type -> Type) comp layers :: Constraint where
    CompIface ctx m comp '[]       = ()
    CompIface ctx m comp (l ': ls) = ( ctx (Component comp) l m
                                     , CompIface ctx m comp ls
                                     )


-- === Attributes === --

type family MapCtx ctx (m :: Type -> Type) attrs :: Constraint where
    MapCtx ctx m '[]       = ()
    MapCtx ctx m (a ': as) = (ctx a m, MapCtx ctx m as)
