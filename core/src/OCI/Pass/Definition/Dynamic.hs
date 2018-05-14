{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition.Dynamic where

import Prologue

import qualified Data.Graph.Class                as Graph
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Data.TypeMap.Strict             as TypeMap
import qualified OCI.Pass.Definition.Class       as Pass
import qualified OCI.Pass.Definition.Declaration as Pass
import qualified OCI.Pass.State.Attr             as Attr
import qualified OCI.Pass.State.Encoder          as Encoder

import Control.Monad.Exception   (Throws)
import Data.Graph.Class          (Graph)
import Data.Map.Strict           (Map)
import Data.Set                  (Set)
import GHC.Exts                  (Any)
import OCI.Pass.Definition.Class (Pass)
import OCI.Pass.State.IRInfo     (CompiledIRInfo)



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

class DescAttrs (t :: Type -> Pass.Property) pass where
    descAttrs :: Lens' Desc IODesc -> Desc -> Desc

instance DescAttrs t Impossible where
    descAttrs _ _ = impossible

instance ( attrs  ~ Pass.Spec pass (t Pass.Attrs)
         , TypeableMany attrs
         ) => DescAttrs t pass where
    descAttrs f = (f . attrs) .~ Attr.reps @attrs
    {-# INLINE descAttrs #-}


-- === Comps === --

class DescComps (t :: Type -> Pass.Property) pass (comps :: [Type]) where
    descComps :: Lens' Desc IODesc -> Desc -> Desc

instance DescComps t pass '[] where
    descComps _ = id ; {-# INLINE descComps #-}

instance ( layers ~ Pass.Spec pass (t comp)
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

data DynamicPass graph = DynamicPass
    { _desc   :: !Desc
    , _runner :: !(AttrVals -> TypeMap.TypeMap (Graph.StateData Graph.Luna) -> IO AttrVals)
    }
makeLenses ''DynamicPass


-- === Instances === --

instance Show (DynamicPass graph) where
    show _ = "DynamicPass" ; {-# INLINE show #-}

instance Mempty Desc where
    mempty = Desc mempty mempty mempty ; {-# INLINE mempty #-}

instance Mempty IODesc where
    mempty = IODesc mempty mempty ; {-# INLINE mempty #-}


-- -- === API === --

-- type Compile pass m =
--     ( Encoder.Encoder        pass
--     , Encoder.AttrEncoder    pass
--     , Encoder.OutAttrDecoder pass
--     , Known pass
--     , Throws Encoder.Error m
--     )

-- compile :: ∀ graph pass m. (Compile pass m)
--         => Pass pass (Graph graph) () -> CompiledIRInfo -> m (DynamicPass graph)
-- compile !pass !cfg = do
--     !s <- Encoder.run @pass cfg
--     let !desc         = describe @pass
--         runner !attrs = do
--             !s' <- Pass.exec pass $! Encoder.encodeAttrs (unwrap attrs) s
--             pure . wrap $ Encoder.decodeOutAttrs s'
--     pure $! DynamicPass desc runner
-- {-# INLINE compile #-}

-- run :: (graph ~ Graph.Luna, Graph.StateEncoder Graph.Luna IO)
--     => DynamicPass graph -> AttrVals -> IO AttrVals
-- run pass attrs = Graph.run $! (pass ^. runner) attrs ; {-# INLINE run #-}




type Compile pass m =
    ( Encoder.Encoder        pass
    , Encoder.AttrEncoder    pass
    , Encoder.OutAttrDecoder pass
    , Known pass
    , Throws Encoder.Error m
    , Graph.StateEncoder Graph.Luna m
    )

compile :: ∀ graph pass m. (Compile pass m, graph ~ Graph.Luna)
        => Pass pass (Graph graph) () -> CompiledIRInfo -> m (DynamicPass graph)
compile !pass !cfg = do
    !s <- Encoder.run @pass cfg
    let !desc         = describe @pass
        runner !attrs !tm = do
            let !x = Encoder.encodeAttrs (unwrap attrs) s
                !y = Pass.exec pass x
            -- !s <- Graph.encodeState @graph
            !s' <- Graph.run2 y tm
            pure . wrap $! Encoder.decodeOutAttrs s'
    pure $! DynamicPass desc runner
{-# INLINE compile #-}

run :: ∀ graph. (Graph.StateEncoder graph IO, graph ~ Graph.Luna) => DynamicPass graph -> AttrVals -> IO AttrVals
run pass attrs = do
    !s <- Graph.encodeState @graph
    !out <- (pass ^. runner) attrs s
    pure out
{-# INLINE run #-}
