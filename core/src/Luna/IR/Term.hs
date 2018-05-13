{-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Luna.IR.Term (module Luna.IR.Term, module X) where

import Prologue

import Luna.IR.Term.Ast       as X
import Luna.IR.Term.Core      as X
import Luna.IR.Term.Format    as X (Ast, Draft, Literal, Phrase, Thunk, Value)
import Luna.IR.Term.Instances as X ()
import Luna.IR.Term.Literal   as X

import qualified Data.Construction                  as Data
import qualified Data.Generics.Traversable          as GTraversable
import qualified Data.Graph.Class                   as Graph
import qualified Data.Graph.Component.Edge          as Link
import qualified Data.Graph.Component.Node.Class    as Term
import qualified Data.Graph.Data.Component.Provider as Component
import qualified Data.Graph.Traversal.Discovery     as Discovery
import qualified OCI.IR.Term.Definition             as Term

import Data.Generics.Traversable (GTraversable)
import OCI.IR.Link.Class         (Link)



----------------------
-- === Uni Term === --
----------------------

-- | UniTerm is the collection of all possible IR terms.
--   The terms are discovered automatically and the UniTerm is generated below.
--   For more information, please refer to the TH funciton documentation.

-- === Definition === -

Term.makeUniTerm
type instance Term.Uni = UniTerm


-- === Instances === --

instance Component.Provider1 tag Link => Component.Provider1 tag UniTerm where
    componentsIO1 = Component.gcomponents @tag ; {-# INLINE componentsIO1 #-}

instance Component.DynamicProvider1 UniTerm where
    dynamicComponentsIO1 = Component.gdynamicComponents ; {-# INLINE dynamicComponentsIO1 #-}

instance StyledShow Term.TagOnly (UniTerm a) where
    styledShow _ = GTraversable.gfoldl' @Term.ShowTag f mempty where
        f acc a = acc <> Term.showTag a

instance (MonadIO m, ctx ~ Data.ShallowDestructor m)
      => Data.ShallowDestructor1 m UniTerm where
    destructShallow1 = GTraversable.gmapM_ @(GTraversable ctx)
                     $ GTraversable.gmapM_ @ctx Data.destructShallow
    {-# INLINE destructShallow1 #-}


instance (Monad m, Discovery.LayersFoldableBuilder__ Discovery.Discovery (Graph.DiscoverComponentLayers m Link.Edges) m)
      => Discovery.Foldable1 Discovery.Discovery m UniTerm where
    fold1 = Discovery.gfold @Discovery.Discovery ; {-# INLINE fold1 #-}
