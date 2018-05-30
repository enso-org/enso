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

import qualified Control.Monad.State.Layered        as State
import qualified Data.Construction                  as Data
import qualified Data.Generics.Traversable          as GTraversable
import qualified Data.Graph.Component.Edge          as Link
import qualified Data.Graph.Component.Node.Class    as Term
import qualified Data.Graph.Data.Component.Set      as ComponentSet
import qualified Data.Graph.Data.Component.Vector   as ComponentVector
import qualified Data.Graph.Data.Graph.Class        as Graph
import qualified Data.Graph.Data.Layer.Class        as Layer
import qualified Data.Graph.Storable.External       as External
import qualified Data.Graph.Traversal.Fold          as Fold
import qualified Data.Graph.Traversal.Scoped        as Fold
import qualified Data.Graph.Traversal.SubComponents as Component
import qualified Data.Graph.Traversal.SubTree       as SubTree
import qualified Foreign.DynamicStorable            as Dynamic
import qualified Foreign.Storable                   as Storable
import qualified Foreign.Storable.Utils             as Storable
import qualified OCI.IR.Term.Definition             as Term

import Control.Monad.State.Layered     (State)
import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import Foreign.Ptr.Utils               (SomePtr)
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



------------------
-- === Fold === --
------------------

-- === Definition === --

instance
    ( Monad m
    , Fold.Builder1 t m (Component Link.Edges)
    , Fold.Builder1 t m (ComponentVector.Vector Link.Edges)
    , Fold.Builder1 t m (ComponentSet.Set       Link.Edges)
    ) => Fold.Builder1 (Fold.Struct t) m UniTerm where
    build1 = gbuildFold__ @t
    {-# INLINE build1 #-}


-- === Internal === --

data UniTermFold t
type instance Fold.Result (UniTermFold t) = Fold.Result t
type GUniTermFold t m = GTraversable (Fold.Builder (UniTermFold t) m)

gbuildFold__ :: ∀ t a m. (GUniTermFold t m a, Applicative m)
             => a -> m (Fold.Result t) -> m (Fold.Result t)
gbuildFold__ = GTraversable.gfoldl' @(Fold.Builder (UniTermFold t) m)
               (\r d -> r . Fold.build @(UniTermFold t) d) id
{-# INLINE gbuildFold__ #-}

instance {-# OVERLAPPABLE #-} (Monad m, Fold.Builder (Fold.Struct (UniTermFold t)) m a)
      => Fold.Builder (UniTermFold t) m a where
    build = Fold.build @(Fold.Struct (UniTermFold t))
    {-# INLINE build #-}

instance Fold.Builder1 t m (Component comp)
      => Fold.Builder (UniTermFold t) m (Component comp layout) where
    build = Fold.build1 @t
    {-# INLINE build #-}

instance Fold.Builder1 t m (ComponentVector.Vector comp)
      => Fold.Builder (UniTermFold t) m (ComponentVector.Vector comp layout) where
    build = Fold.build1 @t
    {-# INLINE build #-}

instance Fold.Builder1 t m (ComponentSet.Set comp)
      => Fold.Builder (UniTermFold t) m (ComponentSet.Set comp layout) where
    build = Fold.build1 @t
    {-# INLINE build #-}

instance (Monad m, GUniTermFold t m (Constructor comp layout))
      => Fold.Builder (UniTermFold t) m (Constructor comp layout) where
    build = gbuildFold__ @t
    {-# INLINE build #-}



------------------------------
-- === ExternalStorable === --
------------------------------

-- === Size discovery === --

-- data UniTermExternalSizeDiscovery
-- type instance Fold.Result UniTermExternalSizeDiscovery = Int

-- instance External.ExternalSizeBuilder1 UniTerm where
--     externalSizeBuilder1 = Fold.build1 @(Fold.Struct UniTermExternalSizeDiscovery)
--     {-# INLINE externalSizeBuilder1 #-}

-- -- FIXME: make these 2 instances nicer? merge?
-- instance Fold.Builder1 UniTermExternalSizeDiscovery IO (Component comp) where
--     build1 = External.sb1
--     {-# INLINE build1 #-}

-- instance Fold.Builder1 UniTermExternalSizeDiscovery IO (ComponentVector.Vector comp) where
--     build1 = External.sb1
--     {-# INLINE build1 #-}

-- instance Fold.Builder1 UniTermExternalSizeDiscovery IO (ComponentSet.Set comp) where
--     build1 = External.sb1
--     {-# INLINE build1 #-}

-- instance External.SB a
--       => Fold.Builder UniTermExternalSizeDiscovery IO a where
--     build = External.sb
--     {-# INLINE build #-}


-- === Dump / load === --

instance External.ExternalStorable (UniTerm layout) where
    loadBuilder = \ptr mdynPtr -> do
        dynPtr <- mdynPtr
        uni    <- Storable.peek ptr
        (!uni', dynPtr') <- flip (State.runT @SomePtr) dynPtr
            $ GTraversable.gtraverse @External.ExternalFieldStorable (\cons -> do
                dynPtr <- State.get @SomePtr
                (!cons', !dynPtr') <- liftIO $ External.loadFieldBuilder
                    $ pure (cons, dynPtr)
                State.put @SomePtr dynPtr'
                pure cons') uni
        Storable.poke ptr uni'
        pure dynPtr'
    {-# INLINE loadBuilder #-}

-- instance External.ExternalSizeBuilder (UniTerm layout) where
--     externalSizeBuilder = GTraversable.gfoldl' @External.SB
--         (\f a -> f . External.sb a) id
--     {-# INLINE externalSizeBuilder #-}


-- === Constructor === --

instance (GTraversable External.ExternalFieldStorable (Constructor comp layout))
      => External.ExternalFieldStorable (Constructor comp layout) where
    loadFieldBuilder = \mdata -> do
        (!cons, !dynPtr) <- mdata
        flip (State.runT @SomePtr) dynPtr
            $ GTraversable.gtraverse @External.ExternalFieldStorable (\a -> do
                dynPtr <- State.get @SomePtr
                (!a', !dynPtr') <- liftIO $ External.loadFieldBuilder
                                 $ pure (a, dynPtr)
                State.put @SomePtr dynPtr'
                pure a') cons
    {-# INLINE loadFieldBuilder #-}

    dumpFieldBuilder = \cons mdynPtr -> do
        dynPtr <- mdynPtr
        flip (State.execT @SomePtr) dynPtr
            $ GTraversable.gtraverse @External.ExternalFieldStorable (\a -> do
                dynPtr  <- State.get @SomePtr
                dynPtr' <- liftIO $ External.dumpFieldBuilder a (pure dynPtr)
                State.put @SomePtr dynPtr'
                pure a) cons
    {-# INLINE dumpFieldBuilder #-}



-- instance (GTraversable External.ExternalSizeBuilder (Constructor comp layout))
--       => External.ExternalSizeBuilder (Constructor comp layout) where
--     externalSizeBuilder = GTraversable.gfoldl' @External.ExternalSizeBuilder
--         (\f a -> f . External.externalSizeBuilder a) id
--     {-# INLINE externalSizeBuilder #-}



type instance Storable.Dynamics (Component comp) = 'Storable.Static

-- FIXME
type instance Storable.Dynamics UniTerm = 'Storable.Dynamic
type instance Storable.Dynamics (Constructor comp layout) = 'Storable.Dynamic
