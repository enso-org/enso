{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Dynamic where

import Prologue

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified OCI.Pass.Definition as Pass
import qualified OCI.Pass.Encoder    as Encoder

import Data.Map.Strict     (Map)
import Data.Set            (Set)
import GHC.Exts            (Any)
import OCI.Pass.Definition (Pass)


---------------------
-- === Dynamic === --
---------------------

-- | Dynamic Passes are just like regular passes but with all the information
--   encoded in values instead of type level. Every pass is converted to its
--   dynamic form before execution. However, the encoded information is used
--   only by the pass manager to properly schedule the pass and update
--   attributes. It is not used to access components and layers. Such access
--   is optimized during compilation time to raw memory read and write.

-- === Definition === --

data DynamicPass = DynamicPass
    { _desc   :: !Desc
    , _runner :: !(AttrMap -> IO ())
    }

data Desc = Desc
    { _inputs  :: IODesc
    , _outputs :: IODesc
    }

data IODesc = IODesc
    { _comps :: Map SomeTypeRep LayerDesc
    , _attrs :: AttrDesc
    }

type    LayerDesc = Set SomeTypeRep
type    AttrDesc  = Set SomeTypeRep
newtype AttrMap   = AttrMap (Map SomeTypeRep Any)


-- === Instances === --

instance Show DynamicPass where
    show _ = "DynamicPass" ; {-# INLINE show #-}

instance Mempty Desc where
    mempty = Desc mempty mempty ; {-# INLINE mempty #-}

instance Mempty IODesc where
    mempty = IODesc mempty mempty ; {-# INLINE mempty #-}

makeLenses ''DynamicPass
makeLenses ''Desc
makeLenses ''IODesc
makeLenses ''AttrMap


-- === API === --

compile :: ∀ pass a m.
    ( Encoder.EncoderX    pass m
    , Encoder.AttrEncoder pass
    , Known pass
    ) => Pass pass a -> Encoder.State -> m DynamicPass
compile !pass !cfg = do
    !staticData <- Encoder.run @pass cfg
    let !desc         = describe @pass
        runner !attrs = Pass.run pass
                     $! Encoder.encodeAttrs (unwrap attrs) staticData
    pure $! DynamicPass desc runner
{-# INLINE compile #-}

run :: DynamicPass -> AttrMap -> IO ()
run = view runner ; {-# INLINE run #-}


-- === Description === --

class    Known pass       where describe :: Desc
instance Known Impossible where describe = impossible
instance {-# OVERLAPPABLE #-}
    ( comps ~ Pass.Vars pass Pass.Elems
    , DescAttrs Pass.In pass
    , DescAttrs Pass.Out pass
    , DescComps Pass.In pass comps
    , DescComps Pass.Out pass comps
    ) => Known pass where
    describe = descAttrs @Pass.In  @pass inputs
             . descAttrs @Pass.Out @pass outputs
             . descComps @Pass.In  @pass @comps inputs
             . descComps @Pass.Out @pass @comps outputs
             $ mempty
    {-# INLINE describe #-}



class DescAttrs (t :: Type -> Pass.Property) pass where
    descAttrs :: Lens' Desc IODesc -> Desc -> Desc

instance DescAttrs t Impossible where
    descAttrs _ _ = impossible

instance ( attrs  ~ Pass.Spec pass (t Pass.Attrs)
         , Typeables attrs
         ) => DescAttrs t pass where
    descAttrs f = (f . attrs) .~ Set.fromList (someTypeReps @attrs)
    {-# INLINE descAttrs #-}



class DescComps (t :: Type -> Pass.Property) pass (comps :: [Type]) where
    descComps :: Lens' Desc IODesc -> Desc -> Desc

instance DescComps t pass '[] where
    descComps _ = id ; {-# INLINE descComps #-}

instance ( layers ~ Pass.Spec pass (t comp)
         , Typeable  comp
         , Typeables layers
         , DescComps t pass comps
         ) => DescComps t pass (comp ': comps) where
    descComps f = trans . descComps @t @pass @comps f where
        trans     = (f . comps) %~ Map.insert compType layers
        compType  = someTypeRep @comp
        layers    = Set.fromList (someTypeReps @layers)
    {-# INLINE descComps #-}


-- class    DescComps pass (comps :: [Type]) where descComps :: Desc -> Desc
-- instance DescComps pass '[]               where descComps = id ; {-# INLINE descComps #-}
-- instance ( isIn      ~ List.In comp (Pass.Ins  pass Pass.Elems)
--          , isOut     ~ List.In comp (Pass.Outs pass Pass.Elems)
--          , inLayers  ~ Pass.Ins pass comp
--          , outLayers ~ Pass.Ins pass comp
--          , Type.Knowns '[isIn, isOut]
--          , Typeable  comp
--          , Typeables inLayers
--          , Typeables outLayers
--          , DescComps pass comps
--          ) => DescComps pass (comp ': comps) where
--     descComps = trans . descComps @pass @comps where
--         trans     = comps %~ Map.insert compType compDesc
--         compType  = someTypeRep @comp
--         compDesc  = CompDesc compIO $ descIOMap inLayers outLayers
--         compIO    = IODesc (Type.from @isIn) (Type.from @isOut)
--         inLayers  = someTypeReps @inLayers
--         outLayers = someTypeReps @outLayers
--     {-# INLINE descComps #-}

-- descIOMap :: [SomeTypeRep] -> [SomeTypeRep] -> Map SomeTypeRep LayerDesc
-- descIOMap ins outs = map where
--     map   = flip (foldr ($)) updts
--           $ Map.fromList . zip ins . repeat $ IODesc True False
--     updts = flip (Map.insertWith (<>)) (IODesc False True) <$> outs
-- {-# INLINE descIOMap #-}
