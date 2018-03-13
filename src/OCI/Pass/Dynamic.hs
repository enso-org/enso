{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Dynamic where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified OCI.Pass.Definition         as Pass
import qualified OCI.Pass.Encoder            as Encoder
import qualified Type.Data.List              as List
import qualified Type.Known                  as Type

import Data.Map.Strict     (Map)
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
    , _runner :: !([Any] -> IO ())
    }

data Desc = Desc
    { _comps :: !(Map SomeTypeRep CompDesc)
    , _attrs :: !(Map SomeTypeRep AttrDesc)
    } deriving (Show)

data CompDesc = CompDesc
    { _io     :: !IODesc
    , _layers :: !(Map SomeTypeRep LayerDesc)
    } deriving (Show)

type LayerDesc = IODesc
type AttrDesc  = IODesc
data IODesc = IODesc
    { _isInput  :: !Bool
    , _isOutput :: !Bool
    } deriving (Show)


-- === Instances === --

instance Show DynamicPass where
    show _ = "DynamicPass" ; {-# INLINE show #-}

instance Default Desc where
    def = Desc def def ; {-# INLINE def #-}

instance Semigroup IODesc where
    IODesc i o <> IODesc i' o' = IODesc (i || i') (o || o') ; {-# INLINE (<>) #-}

makeLenses ''DynamicPass
makeLenses ''Desc
makeLenses ''CompDesc
makeLenses ''IODesc


-- === API === --

-- uncheckedRunDynamic :: DynamicPass -> IO ()
-- uncheckedRunDynamic = view runner ; {-# INLINE uncheckedRunDynamic #-}

-- compile :: ∀ pass a. Known pass => Pass.State pass -> Pass pass a -> DynamicPass
-- compile !s !p = DynamicPass (describe @pass) . void $ flip State.evalT s (coerce p) ; {-# INLINE compile #-}

-- compile2 :: ∀ pass a. Known pass => Pass pass a -> Any -> DynamicPass
-- compile2 !p !s = compile (unsafeCoerce s) p ; {-# INLINE compile2 #-}

runPass :: ∀ pass a. Pass.State pass -> Pass pass a -> IO ()
runPass !state !pass = void $ State.evalT (coerce pass) state ; {-# INLINE runPass #-}


compileX :: ∀ pass a m.
    ( Encoder.EncoderX    pass m
    , Encoder.AttrEncoder pass
    , Known pass
    ) => Encoder.State -> Pass pass a -> m DynamicPass
compileX !cfg !pass = do
    !staticData <- Encoder.run @pass cfg
    let !desc = describe @pass
        runner !attrs = runPass passData pass where
            !passData = Encoder.encodeAttrs attrs staticData
    pure $! DynamicPass desc runner

-- exec :: State pass -> Pass pass a -> IO (State pass)
-- exec !s p = flip State.execT s (coerce p) ; {-# INLINE exec #-}



class    Known pass       where describe :: Desc
instance Known Impossible where describe = impossible
instance {-# OVERLAPPABLE #-}
    ( comps ~ Pass.Vars pass Pass.Elems
    , DescAttrs pass
    , DescComps pass comps
    ) => Known pass where
    describe = descAttrs @pass . descComps @pass @comps $ def ; {-# INLINE describe #-}

class    DescAttrs pass       where descAttrs :: Desc -> Desc
instance DescAttrs Impossible where descAttrs = impossible
instance ( inAttrs  ~ Pass.Ins  pass Pass.Attrs
         , outAttrs ~ Pass.Outs pass Pass.Attrs
         , Typeables inAttrs
         , Typeables outAttrs
         ) => DescAttrs pass where
    descAttrs = attrs .~ descIOMap ins outs where
        ins  = someTypeReps @inAttrs
        outs = someTypeReps @outAttrs
    {-# INLINE descAttrs #-}

class    DescComps pass (comps :: [Type]) where descComps :: Desc -> Desc
instance DescComps pass '[]               where descComps = id ; {-# INLINE descComps #-}
instance ( isIn      ~ List.In comp (Pass.Ins  pass Pass.Elems)
         , isOut     ~ List.In comp (Pass.Outs pass Pass.Elems)
         , inLayers  ~ Pass.Ins pass comp
         , outLayers ~ Pass.Ins pass comp
         , Type.Knowns '[isIn, isOut]
         , Typeable  comp
         , Typeables inLayers
         , Typeables outLayers
         , DescComps pass comps
         ) => DescComps pass (comp ': comps) where
    descComps = trans . descComps @pass @comps where
        trans     = comps %~ Map.insert compType compDesc
        compType  = someTypeRep @comp
        compDesc  = CompDesc compIO $ descIOMap inLayers outLayers
        compIO    = IODesc (Type.from @isIn) (Type.from @isOut)
        inLayers  = someTypeReps @inLayers
        outLayers = someTypeReps @outLayers

descIOMap :: [SomeTypeRep] -> [SomeTypeRep] -> Map SomeTypeRep LayerDesc
descIOMap ins outs = map where
    map   = flip (foldr ($)) updts
          $ Map.fromList . zip ins . repeat $ IODesc True False
    updts = flip (Map.insertWith (<>)) (IODesc False True) <$> outs

