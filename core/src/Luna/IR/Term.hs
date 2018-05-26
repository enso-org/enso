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
import qualified Data.Graph.Component.Edge          as Link
import qualified Data.Graph.Component.Node.Class    as Term
import qualified Data.Graph.Data.Graph.Class        as Graph
import qualified Data.Graph.Traversal.Fold          as Fold
import qualified Data.Graph.Traversal.SubComponents as Component
import qualified Data.Graph.Traversal.SubTree       as SubTree
import qualified Foreign.PartitionStorable          as PartitionStorable
import qualified OCI.IR.Term.Definition             as Term

import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import Foreign.PartitionStorable       (PartitionStorable)
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

instance StyledShow Term.TagOnly (UniTerm a) where
    styledShow _ = GTraversable.gfoldl' @Term.ShowTag f mempty where
        f acc a = acc <> Term.showTag a

instance (MonadIO m, ctx ~ Data.ShallowDestructor m)
      => Data.ShallowDestructor1 m UniTerm where
    destructShallow1 = GTraversable.gmapM_ @(GTraversable ctx)
                     $ GTraversable.gmapM_ @ctx Data.destructShallow
    {-# INLINE destructShallow1 #-}



-------------------------
-- === FoldBuilder === --
-------------------------

-- === Definition === --

instance (Monad m, Fold.Builder1 t m (Component Link.Edges))
      => Fold.Builder1 t m UniTerm where
    build1 = gbuildFold__ @t
    {-# INLINE build1 #-}


-- === Internal === --

class UniTermFold t m a where
    buildFold__ :: a -> m (Fold.Result t) -> m (Fold.Result t)

gbuildFold__ :: ∀ t a m. (GTraversable (UniTermFold t m) a, Applicative m)
      => a -> m (Fold.Result t) -> m (Fold.Result t)
gbuildFold__ = GTraversable.gfoldl' @(UniTermFold t m) (\r d x -> r $! buildFold__ @t d x) id
{-# INLINE gbuildFold__ #-}

instance {-# OVERLAPPABLE #-} (Fold.Builder t m a)
      => UniTermFold t m a where
    buildFold__ = Fold.build @t
    {-# INLINE buildFold__ #-}

instance Fold.Builder1 t m (Component comp)
      => UniTermFold t m (Component comp layout) where
    buildFold__ = Fold.build1 @t
    {-# INLINE buildFold__ #-}

instance (Monad m, GTraversable (UniTermFold t m) (Constructor comp layout))
      => UniTermFold t m (Constructor comp layout) where
    buildFold__ = gbuildFold__ @t
    {-# INLINE buildFold__ #-}

