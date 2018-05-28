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
import qualified Data.Graph.Data.Graph.Class        as Graph
import qualified Data.Graph.Data.Layer.Class        as Layer
import qualified Data.Graph.Storable.External       as External
import qualified Data.Graph.Traversal.Fold          as Fold
import qualified Data.Graph.Traversal.Scoped        as Fold
import qualified Data.Graph.Traversal.SubComponents as Component
import qualified Data.Graph.Traversal.SubTree       as SubTree
import qualified Foreign.DynamicStorable            as Dynamic
import qualified Foreign.Storable                   as Storable
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

instance (Monad m, Fold.Builder1 t m (Component Link.Edges))
      => Fold.Builder1 t m UniTerm where
    build1 = gbuildFold__ @t
    {-# INLINE build1 #-}


-- === Internal === --

class UniTermFold t m a where
    buildFold__ :: a -> m (Fold.Result t) -> m (Fold.Result t)

gbuildFold__ :: ∀ t a m. (GTraversable (UniTermFold t m) a, Applicative m)
             => a -> m (Fold.Result t) -> m (Fold.Result t)
gbuildFold__ = GTraversable.gfoldl' @(UniTermFold t m)
               (\r d x -> r $! buildFold__ @t d x) id
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



------------------------------
-- === ExternalStorable === --
------------------------------

-- === Size discovery === --

data UniTermExternalSizeDiscovery
type instance Fold.Result UniTermExternalSizeDiscovery = Int

instance External.Measured1 UniTerm where
    size1 = \a -> Fold.build1 @UniTermExternalSizeDiscovery a (pure 0)
    {-# INLINE size1 #-}

instance MonadIO m
      => Fold.Builder1 UniTermExternalSizeDiscovery m (Component comp) where
    build1 a msize = (+) <$> msize <*> liftIO (External.size1 a)
    {-# INLINE build1 #-}

instance (External.Measured a, MonadIO m)
      => Fold.Builder UniTermExternalSizeDiscovery m a where
    build a msize = (+) <$> msize <*> liftIO (External.size a)
    {-# INLINE build #-}


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

instance External.Measured (UniTerm layout) where
    size = GTraversable.gfoldlM @External.Measured
        (\i a -> (i +) <$> External.size a) 0
    {-# INLINE size #-}

instance (GTraversable External.Measured (Constructor comp layout))
      => External.Measured (Constructor comp layout) where
    size = GTraversable.gfoldlM @External.Measured
        (\i a -> (i +) <$> External.size a) 0
    {-# INLINE size #-}







-- data ExternalSizeDiscovery

-- type instance Fold.Result     ExternalSizeDiscovery = Int
-- type instance Fold.LayerScope ExternalSizeDiscovery = 'Fold.All

-- compExternalSize :: (Fold.Builder (Fold.Scoped ExternalSizeDiscovery) m (Component comp layout))
--                  => Component comp layout -> m Int
-- compExternalSize a = Fold.build @(Fold.Scoped ExternalSizeDiscovery) a (pure 0)

-- instance (MonadIO m, External.Measured1 (Layer.Cons layer) )
--       => Fold.LayerBuilder ExternalSizeDiscovery m layer where
--     layerBuild = \layer msize -> (+) <$> msize <*> liftIO (External.size1 layer)


