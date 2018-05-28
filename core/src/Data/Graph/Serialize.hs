module Data.Graph.Serialize where

import Prologue

import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.List       as Component
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Deep            as Deep
import qualified Data.Graph.Traversal.Fold            as Fold
import qualified Data.Graph.Traversal.Scoped          as Fold
import qualified Data.Graph.Storable.External             as ExternalStorable

import Data.Graph.Component.Edge.Class       (Source)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Storable.External              (ExternalStorable)
import Foreign.Ptr                           (Ptr, plusPtr)
import Foreign.Ptr.Utils                     (SomePtr)
import Foreign.Storable1                     (Storable1)


-- -----------------------
-- -- === Discovery === --
-- -----------------------

-- -- === Definition === --

-- data Dump (scope :: Fold.Scope)
-- type instance Fold.Result     (Dump scope) = Component.List'
-- type instance Fold.LayerScope (Dump scope) = scope


-- -- === API === --

-- type Dumpable  scope = Deep.Builder  (Dump scope)
-- type Dumpable1 scope = Deep.Builder1 (Dump scope)

-- dump  :: ∀ scope m a.   Dumpable  scope m a => a   -> m Component.List'
-- dump1 :: ∀ scope m a t. Dumpable1 scope m a => a t -> m Component.List'
-- dump  = Deep.run  @(Dump scope)
-- dump1 = Deep.run1 @(Dump scope)
-- {-# INLINE dump #-}
-- {-# INLINE dump1 #-}


-- -- === Simple === --

-- -- type SimpleDiscoveryScope = 'Fold.Whitelist '[Model, Type.Type, Source]
-- -- type Dumpable'   = Dumpable  SimpleDiscoveryScope
-- -- type Dumpable1'  = Dumpable1 SimpleDiscoveryScope

-- -- dump'  :: ∀ m a.   Dumpable'  m a => a   -> m Component.List'
-- -- dump1' :: ∀ m a t. Dumpable1' m a => a t -> m Component.List'
-- -- dump'  = dump  @SimpleDiscoveryScope
-- -- dump1' = dump1 @SimpleDiscoveryScope
-- -- {-# INLINE dump'  #-}
-- -- {-# INLINE dump1' #-}


-- -- === Instances === --

-- instance Monad m => Fold.LayerBuilder (Dump scope) m layer where
--     layerBuild = \layer macc -> do

--         (Component.Cons $ Layout.relayout cmp) <$> acc
--     {-# INLINE componentBuild #-}



class ExternalStorableLayers (layers :: [Type]) where
    dumpLayersBuilder :: SomePtr -> IO SomePtr -> IO SomePtr

instance ExternalStorableLayers '[] where
    dumpLayersBuilder = \_ -> id
    {-# INLINE dumpLayersBuilder #-}

instance ( ExternalStorableLayers ls
         , ExternalStorable (Layer.Cons l ()) -- FIXME
         , Storable1 (Layer.Cons l) )
       => ExternalStorableLayers (l ': ls) where
    dumpLayersBuilder ptr mdynPtr = dumpLayersBuilder @ls ptr' mdynPtr' where
        ptr'     = ptr `plusPtr` Layer.byteSize @l
        mdynPtr' = ExternalStorable.dumpBuilder
                   (coerce ptr :: Ptr (Layer.Cons l ())) mdynPtr
    {-# INLINE dumpLayersBuilder #-}

dumpLayers :: ∀ layers. ExternalStorableLayers layers
           => SomePtr -> SomePtr -> IO SomePtr
dumpLayers = \ptr -> dumpLayersBuilder @layers ptr . pure
{-# INLINE dumpLayers #-}

type ExternalStorableComponent comp m =
    ( MonadIO m
    , ExternalStorableLayers (Graph.DiscoverComponentLayers m comp)
    )

dumpComponent :: ∀ comp m layout. ExternalStorableComponent comp m
              => Component comp layout -> SomePtr -> m SomePtr
dumpComponent = \comp dynPtr -> liftIO
    $! dumpLayers @(Graph.DiscoverComponentLayers m comp) (coerce comp) dynPtr
{-# INLINE dumpComponent #-}
