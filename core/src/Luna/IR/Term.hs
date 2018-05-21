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

import qualified Data.Construction               as Data
import qualified Data.Generics.Traversable       as GTraversable
import qualified Data.Graph.Component.Edge       as Link
import qualified Data.Graph.Component.Node.Class as Term
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Traversal.Component  as Component
import qualified Data.Graph.Traversal.Fold       as Fold
import qualified Data.Graph.Traversal.Provider   as Component
import qualified Data.Graph.Traversal.SubTree    as SubTree
import qualified OCI.IR.Term.Definition          as Term

import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import OCI.IR.Link.Class               (Link)


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

-- instance (Component.Provider1 tag m Link, Monad m) => Component.Provider1 tag m UniTerm where
--     gather1 = Component.ggather @tag ; {-# INLINE gather1 #-}

-- instance Component.DynamicProvider1 UniTerm where
--     dynamicComponentsIO1 = Component.gdynamicComponents ; {-# INLINE dynamicComponentsIO1 #-}

instance StyledShow Term.TagOnly (UniTerm a) where
    styledShow _ = GTraversable.gfoldl' @Term.ShowTag f mempty where
        f acc a = acc <> Term.showTag a

instance (MonadIO m, ctx ~ Data.ShallowDestructor m)
      => Data.ShallowDestructor1 m UniTerm where
    destructShallow1 = GTraversable.gmapM_ @(GTraversable ctx)
                     $ GTraversable.gmapM_ @ctx Data.destructShallow
    {-# INLINE destructShallow1 #-}


-- instance (Monad m, Fold.LayersFoldableBuilder__ SubTree.SubTreeDiscovery (Graph.DiscoverComponentLayers m Link.Edges) m)
--       => Fold.Foldable1 SubTree.SubTreeDiscovery m UniTerm where
--     buildFold1 = Fold.gbuildFold @SubTree.SubTreeDiscovery ; {-# INLINE buildFold1 #-}

-- FIXME: remove MonadIO
-- instance (MonadIO m
--          , GTraversable (Fold.Foldable (Component.ComponentDiscovery comp) m) (UniTerm a)
--          , Fold.Foldable1 (Component.ComponentDiscovery comp) m (Component Link.Edges)
--          , Fold.Foldable1 (Component.ComponentDiscovery comp) m (Component Term.Nodes)
--          )
--       => Fold.Foldable1 (Component.ComponentDiscovery comp) m UniTerm where
--     buildFold1 = Fold.gbuildFold @(Component.ComponentDiscovery comp) ; {-# INLINE buildFold1 #-}



class UniTermFold t m a where
    buildUniTermFold :: a -> m (Fold.Result t) -> m (Fold.Result t)

gbuildUniTermFold :: ∀ t a m. (GTraversable (UniTermFold t m) a, Applicative m)
      => a -> m (Fold.Result t) -> m (Fold.Result t)
gbuildUniTermFold = GTraversable.gfoldl' @(UniTermFold t m) (\r d x -> r $! buildUniTermFold @t d x) id
{-# INLINE gbuildUniTermFold #-}

instance ( Monad m
         , Fold.Foldable1 t m (Component Link.Edges)
         )
      => Fold.Foldable1 t m UniTerm where
    buildFold1 = gbuildUniTermFold @t ; {-# INLINE buildFold1 #-}


instance {-# OVERLAPPABLE #-} (Fold.Foldable t m a)
      => UniTermFold t m a where
    buildUniTermFold = Fold.buildFold @t ; {-# INLINE buildUniTermFold #-}

instance Fold.Foldable1 t m (Component comp)
      => UniTermFold t m (Component comp layout) where
    buildUniTermFold = Fold.buildFold1 @t ; {-# INLINE buildUniTermFold #-}

instance (Monad m, GTraversable (UniTermFold t m) (Constructor comp layout))
      => UniTermFold t m (Constructor comp layout) where
    buildUniTermFold = gbuildUniTermFold @t ; {-# INLINE buildUniTermFold #-}
