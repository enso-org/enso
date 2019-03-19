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
import qualified Data.Graph.Fold.Class           as Fold
import qualified Data.Graph.Fold.Struct          as Fold
import qualified Data.Property                   as Property
import qualified Foreign.Storable.Utils          as Storable
import qualified OCI.IR.Term.Definition          as Term
-- import qualified Data.Graph.Store.External        as External
-- import qualified Data.Graph.Store.MemoryRegion as MemoryRegion

import Data.Generics.Traversable             (GTraversable)
import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.Set         (ComponentSet)
import Data.Graph.Data.Component.Vector      (ComponentVector)
import Data.Vector.Storable.Foreign          (Vector)
import OCI.Data.Name                         (Name)
import Data.Mutable.Storable.SmallAutoVector (UnmanagedSmallVector)
-- import Data.Graph.Store.External        (ExternalFieldStorable,
--                                          ExternalStorable)



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
    , Fold.Builder1 t m (ComponentVector Link.Edges)
    , Fold.Builder1 t m (ComponentSet    Link.Edges)
    , Fold.Builder  t m (UnmanagedSmallVector 16 Word8)
    , Fold.Builder  t m (UnmanagedSmallVector 16 Char)
    , Fold.Builder  t m (UnmanagedSmallVector 16 Name) -- x
    , Fold.Builder  t m (Term.List Name)
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

instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder (Fold.Struct (UniTermFold t)) m a)
      => Fold.Builder (UniTermFold t) m a where
    build = Fold.build @(Fold.Struct (UniTermFold t))
    {-# INLINE build #-}

instance Fold.Builder1 t m (Component comp)
      => Fold.Builder (UniTermFold t) m (Component comp layout) where
    build = Fold.build1 @t
    {-# INLINE build #-}

instance Fold.Builder1 t m (ComponentVector comp)
      => Fold.Builder (UniTermFold t) m (ComponentVector comp layout) where
    build = Fold.build1 @t
    {-# INLINE build #-}

instance Fold.Builder1 t m (ComponentSet comp)
      => Fold.Builder (UniTermFold t) m (ComponentSet comp layout) where
    build = Fold.build1 @t
    {-# INLINE build #-}

instance (Monad m, GUniTermFold t m (Constructor comp layout))
      => Fold.Builder (UniTermFold t) m (Constructor comp layout) where
    build = gbuildFold__ @t
    {-# INLINE build #-}

instance Fold.Builder t m (Vector a)
      => Fold.Builder (UniTermFold t) m (Vector a) where
    build = Fold.build @t
    {-# INLINE build #-}

instance Fold.Builder t m (UnmanagedSmallVector n a)
      => Fold.Builder (UniTermFold t) m (UnmanagedSmallVector n a) where
    build = Fold.build @t
    {-# INLINE build #-}



-- ------------------------------
-- -- === ExternalStorable === --
-- ------------------------------

-- -- === Dump / load === --

-- instance ExternalStorable (UniTerm layout) where
--     loadBuilder = \ptr mdynReg -> do
--         dynReg <- mdynReg
--         uni    <- Storable.peek ptr
--         (!uni', dynReg') <- flip (State.runT @MemoryRegion.Dynamic) dynReg
--             $ GTraversable.gtraverse @ExternalFieldStorable (\cons -> do
--                 dynReg <- State.get @MemoryRegion.Dynamic
--                 (!cons', !dynReg') <- liftIO $ External.loadFieldBuilder
--                     $ pure (cons, dynReg)
--                 State.put @MemoryRegion.Dynamic dynReg'
--                 pure cons') uni
--         Storable.poke ptr uni'
--         pure dynReg'
--     {-# INLINE loadBuilder #-}


-- -- === Constructor === --

-- instance (GTraversable ExternalFieldStorable (Constructor comp layout))
--       => ExternalFieldStorable (Constructor comp layout) where
--     loadFieldBuilder = \mdata -> do
--         (!cons, !dynReg) <- mdata
--         flip (State.runT @MemoryRegion.Dynamic) dynReg
--             $ GTraversable.gtraverse @ExternalFieldStorable (\a -> do
--                 dynReg <- State.get @MemoryRegion.Dynamic
--                 (!a', !dynReg') <- liftIO $ External.loadFieldBuilder
--                                  $ pure (a, dynReg)
--                 State.put @MemoryRegion.Dynamic dynReg'
--                 pure a') cons
--     {-# INLINE loadFieldBuilder #-}

--     dumpFieldBuilder = \cons mdynReg -> do
--         dynReg <- mdynReg
--         flip (State.execT @MemoryRegion.Dynamic) dynReg
--             $ GTraversable.gtraverse @ExternalFieldStorable (\a -> do
--                 dynReg  <- State.get @MemoryRegion.Dynamic
--                 dynReg' <- liftIO $ External.dumpFieldBuilder a (pure dynReg)
--                 State.put @MemoryRegion.Dynamic dynReg'
--                 pure a) cons
--     {-# INLINE dumpFieldBuilder #-}


type instance Property.Get Storable.Dynamics (Component comp) = Storable.Static

-- FIXME
type instance Property.Get Storable.Dynamics UniTerm = Storable.Dynamic
type instance Property.Get Storable.Dynamics (Constructor comp layout) = Storable.Dynamic

