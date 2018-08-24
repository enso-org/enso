{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition.Dynamic where

import Prologue

import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Data.TypeMap.Strict             as TypeMap
import qualified OCI.Pass.Definition.Class       as Pass
import qualified OCI.Pass.Definition.Declaration as Pass
import qualified OCI.Pass.State.Attr             as Attr
import qualified OCI.Pass.State.Encoder          as Encoder

import Control.Monad.Exception     (Throws)
import Data.Graph.Data.Graph.Class (Graph)
import Data.Map.Strict             (Map)
import Data.Set                    (Set)
import GHC.Exts                    (Any)
import OCI.Pass.Definition.Class   (Pass)



-------------------------
-- === Description === --
-------------------------

-- === Definition === --

data Desc = Desc
    { _inputs     :: IODesc
    , _outputs    :: IODesc
    , _attrLayout :: [Attr.Rep]
    }

data IODesc = IODesc
    { _comps :: Map SomeTypeRep LayerDesc
    , _attrs :: AttrDesc
    }

type    LayerDesc = Set SomeTypeRep
type    AttrDesc  = [Attr.Rep]
newtype AttrVals  = AttrVals [Any]

makeLenses ''Desc
makeLenses ''IODesc
makeLenses ''AttrVals


-- === Generation === --

class    Known pass       where describe :: Desc
instance Known Impossible where describe = impossible
instance {-# OVERLAPPABLE #-}
    ( comps ~ Pass.Vars pass Pass.Elems
    , attrs ~ Pass.Vars pass Pass.Attrs
    , DescAttrs Pass.In pass
    , DescAttrs Pass.Out pass
    , DescComps Pass.In pass comps
    , DescComps Pass.Out pass comps
    , TypeableMany attrs
    ) => Known pass where
    describe = descAttrs @Pass.In  @pass inputs
             . descAttrs @Pass.Out @pass outputs
             . descComps @Pass.In  @pass @comps inputs
             . descComps @Pass.Out @pass @comps outputs
             . (attrLayout .~ Attr.reps @attrs)
             $ mempty
    {-# INLINE describe #-}


-- === Attrs === --

class DescAttrs (t :: Type -> Type) pass where
    descAttrs :: Lens' Desc IODesc -> Desc -> Desc

instance DescAttrs t Impossible where
    descAttrs _ _ = impossible

instance ( attrs ~ Pass.Resolve t pass Pass.Attrs
         , TypeableMany attrs
         ) => DescAttrs t pass where
    descAttrs f = (f . attrs) .~ Attr.reps @attrs
    {-# INLINE descAttrs #-}


-- === Comps === --

class DescComps (t :: Type -> Type) pass (comps :: [Type]) where
    descComps :: Lens' Desc IODesc -> Desc -> Desc

instance DescComps t pass '[] where
    descComps _ = id ; {-# INLINE descComps #-}

instance ( layers ~ Pass.Resolve t pass comp
         , Typeable  comp
         , TypeableMany layers
         , DescComps t pass comps
         ) => DescComps t pass (comp ': comps) where
    descComps f = trans . descComps @t @pass @comps f where
        trans     = (f . comps) %~ Map.insert compType layers
        compType  = someTypeRep @comp
        layers    = Set.fromList (someTypeReps @layers)
    {-# INLINE descComps #-}



--------------------------
-- === Dynamic Pass === --
--------------------------

-- | Dynamic Passes are just like regular passes but with all the information
--   encoded in values instead of type level. Every pass is converted to its
--   dynamic form before execution. However, the encoded information is used
--   only by the pass manager to properly schedule the pass and update
--   attributes. It is not used to access components and layers. Such access
--   is optimized during compilation time to raw memory read and write.

-- === Definition === --

data DynamicPass = DynamicPass
    { _desc   :: !Desc
    , _runner :: !(AttrVals -> IO AttrVals)
    }
makeLenses ''DynamicPass


-- === Instances === --

instance Show DynamicPass where
    show _ = "DynamicPass" ; {-# INLINE show #-}

instance Mempty Desc where
    mempty = Desc mempty mempty mempty ; {-# INLINE mempty #-}

instance Mempty IODesc where
    mempty = IODesc mempty mempty ; {-# INLINE mempty #-}


-- -- === API === --

type Compile stage pass m =
    ( Encoder.AttrEncoder    pass
    , Encoder.OutAttrDecoder pass
    , Known pass
    , Graph.Monad stage m
    , Default (Pass.State pass)
    -- , Graph.Monad Graph.Luna m
    )

-- | Graph state is evaluated while compiling pass in order to inline its
--   definition. It enables crucial optimizations and is a very sensitive part
--   of the code. Please carefuly watch benchmarks when editing it.
compile :: ∀ stage pass m. Compile stage pass m => Pass stage pass () -> m DynamicPass
compile pass = do
    graphState <- Graph.getState @stage
    let desc = describe @pass
        runner attrs = do
            let attrState = Encoder.encodeAttrs (unwrap attrs) def
                passFunc  = Pass.exec pass attrState
            out <- Graph.eval passFunc graphState
            pure . wrap $! Encoder.decodeOutAttrs out
    pure $! DynamicPass desc runner
{-# INLINE compile #-}

run :: ∀ m. MonadIO m => DynamicPass -> AttrVals -> m AttrVals
run pass attrs = liftIO $! (pass ^. runner) attrs ; {-# INLINE run #-}
