{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.Encoder where

import Prologue

import qualified Data.Graph.Component.Class    as Component
import qualified Data.Graph.Component.Dynamic  as Component
import qualified Data.Graph.Component.Layer    as Layer
import qualified Data.Graph.Component.Provider as Component
import qualified Data.Map                      as Map
import qualified Data.TypeMap.Strict           as TypeMap
import qualified Foreign.Marshal.Alloc         as Mem
import qualified Foreign.Marshal.Utils         as Mem
import qualified Foreign.Memory.Pool           as MemPool
import qualified Foreign.Ptr                   as Ptr
import qualified OCI.Pass.Definition           as Pass
import qualified OCI.Pass.Registry             as Reg

import Control.Monad.Exception    (Throws, throw)
import Data.Graph.Component.Class (Component)
import Data.Map.Strict            (Map)
import Foreign.Info.ByteSize      (ByteSize (ByteSize))
import Foreign.Memory.Pool        (MemPool)
import Foreign.Ptr.Utils          (SomePtr)
import GHC.Exts                   (Any)
import OCI.Pass.Attr              (Attr)



--------------------
-- === Config === --
--------------------

-- | Config is computed once after all components and primitive layers
--   have been registered. It encodes information about memory layout which
--   is constant trough all of compiler passes.


-- === Definition === --

newtype State = State
    { _components :: Map Component.TagRep ComponentInfo
    }

data ComponentInfo = ComponentInfo
    { _byteSize          :: !Int
    , _layers            :: !(Map SomeTypeRep LayerInfo)
    , _layersInitializer :: !SomePtr
    , _layersConstructor :: !(SomePtr -> IO ())
    , _layersDestructor  :: !(SomePtr -> IO ())
    , _layersComponents  :: !(SomePtr -> IO [Component.Dynamic])
    , _memPool           :: !(MemPool ())
    }

newtype LayerInfo = LayerInfo
    { _byteOffset :: Int
    } deriving (Show)

makeLenses ''LayerInfo
makeLenses ''ComponentInfo
makeLenses ''State


-- === Construction === --

computeConfig :: MonadIO m => Reg.State -> m State
computeConfig cfg = wrap <$> mapM computeComponentInfo (cfg ^. Reg.components) ; {-# INLINE computeConfig #-}

computeComponentInfo :: MonadIO m => Reg.ComponentInfo -> m ComponentInfo
computeComponentInfo compCfg = compInfo where
    layerReps     = Map.keys  $ compCfg ^. Reg.layers
    layerInfos    = Map.elems $ compCfg ^. Reg.layers
    layerSizes    = view Reg.byteSize <$> layerInfos
    layerOffsets  = scanl (+) 0 layerSizes
    layerCfgs     = wrap <$> layerOffsets
    layerOffInfos = zip layerOffsets layerInfos
    compSize      = sum layerSizes
    compInfo      = ComponentInfo compSize
                <$> pure (fromList $ zip layerReps layerCfgs)
                <*> prepareLayerInitializer      layerInfos
                <*> prepareLayersConstructor     layerInfos
                <*> prepareLayersDestructor      layerInfos
                <*> prepareSubComponentDiscovery layerOffInfos
                <*> MemPool.new def (MemPool.ItemSize compSize)

prepareLayerInitializer :: MonadIO m => [Reg.LayerInfo] -> m SomePtr
prepareLayerInitializer ls = do
    ptr <- mallocLayerInitializer ls
    liftIO $ fillLayerInitializer ptr ls
    pure ptr

mallocLayerInitializer :: MonadIO m => [Reg.LayerInfo] -> m SomePtr
mallocLayerInitializer = \case
    [] -> pure Ptr.nullPtr
    ls -> liftIO . Mem.mallocBytes . sum $ view Reg.byteSize <$> ls

fillLayerInitializer :: SomePtr -> [Reg.LayerInfo] -> IO ()
fillLayerInitializer = go where
    go ptr = \case
        []     -> pure ()
        (l:ls) -> mapM_ (flip (Mem.copyBytes ptr) byteSize) initializer
               >> go ptr' ls
            where initializer = l ^. Reg.initializer
                  byteSize    = l ^. Reg.byteSize
                  ptr'        = Ptr.plusPtr ptr byteSize

prepareLayersConstructor :: Monad m => [Reg.LayerInfo] -> m (SomePtr -> IO ())
prepareLayersDestructor  :: Monad m => [Reg.LayerInfo] -> m (SomePtr -> IO ())
prepareLayersConstructor = concatLayersIOActions Reg.constructor ; {-# INLINE prepareLayersConstructor #-}
prepareLayersDestructor  = concatLayersIOActions Reg.destructor  ; {-# INLINE prepareLayersDestructor #-}

-- | The function is defined in monad in order to compute the dynamic
--   initializer during monad resolution. Otherwise, the Maybe pattern
--   matching would be deffered to the layer initializer, which will
--   consequently run slower.
concatLayersIOActions :: Monad m
    => Lens' Reg.LayerInfo (Maybe (SomePtr -> IO ()))
    -> [Reg.LayerInfo]
    -> m (SomePtr -> IO ())
concatLayersIOActions lens = go where
    go = \case
        []           -> pure $ const (pure ())
        (!l : (!ls)) -> flip fuse (l ^. lens) =<< go ls where
            !byteSize = l ^. Reg.byteSize
            fuse !g   = \case
                Nothing -> pure $ \(!ptr) ->
                    let !out = g $! Ptr.plusPtr ptr byteSize
                    in  out
                Just f  -> pure $ \(!ptr) ->
                    let !proc1 = f ptr
                        !proc2 = g $! Ptr.plusPtr ptr byteSize
                        !out   = proc1 >> proc2
                    in  out
            {-# INLINE fuse #-}


-- FIXME : check if the above hack applies here too
prepareSubComponentDiscovery :: MonadIO m
    => [(Int, Reg.LayerInfo)] -> m (SomePtr -> IO [Component.Dynamic])
prepareSubComponentDiscovery ls = do
    pure $ (\ptr -> concat <$> mapM (uncurry $ getFromLayer ptr) ls)
    where
    getFromLayer :: SomePtr -> Int -> Reg.LayerInfo -> IO [Component.Dynamic]
    getFromLayer p off l = (l ^. Reg.subComponents) (p `Ptr.plusPtr` off)



---------------------
-- === Encoder === --
---------------------

-- === Errors === --

data EncodingError
    = MissingComponent Component.TagRep
    | MissingLayer     SomeTypeRep
    deriving (Show)

newtype Error = Error (NonEmpty EncodingError)
    deriving (Semigroup, Show)
makeLenses ''Error

type EncoderResult  = Either Error
type EncodingResult = Either EncodingError

instance Exception Error


-- === API === --

type Encoding pass = (Encoder pass, Default (Pass.State pass))

tryRun :: Encoding pass => State -> EncoderResult (Pass.State pass)
tryRun cfg = ($ def) <$> encode cfg ; {-# INLINE tryRun #-}

run :: ∀ pass m. (Encoding pass, Throws Error m)
    => State -> m (Pass.State pass)
run cfg = case tryRun cfg of
    Left  e -> throw e
    Right a -> pure a
{-# INLINE run #-}


-- === Encoding utils === --

encode :: ∀ pass. Encoder pass
       => State -> EncoderResult (Pass.State pass -> Pass.State pass)
encode = encode__ @pass @(EncoderTarget pass) ; {-# INLINE encode #-}

type  Encoder       pass = Encoder__ pass (EncoderTarget pass)
type  EncoderTarget pass = Pass.Vars pass Pass.Elems
class Encoder__     pass (cs :: [Type]) where
    encode__ :: State -> EncoderResult (Pass.State pass -> Pass.State pass)

instance Encoder__ pass '[] where
    encode__ _ = Right id ; {-# INLINE encode__ #-}

instance ( layers      ~ Pass.Vars pass comp
         , targets     ~ Pass.ComponentLayerLayout Pass.LayerByteOffset pass comp
         , compMemPool ~ MemPool (Component comp ())
         , compSize    ~ ByteSize (Component comp)
         , compTravsl  ~ Component.DynamicTraversal comp
         , layerInit   ~ Layer.DynamicManager comp
         , TypeableMany layers
         , Typeable  comp
         , Encoder__ pass comps
         , PassDataElemEncoder  compMemPool pass
         , PassDataElemEncoder  compSize    pass
         , PassDataElemEncoder  compTravsl  pass
         , PassDataElemEncoder  layerInit   pass
         , PassDataElemsEncoder targets Int pass
         ) => Encoder__ pass (comp ': comps) where
    encode__ cfg = encode where
        encode     = appSemiLeft encoder subEncoder
        encoder    = procComp =<< mcomp
        subEncoder = encode__ @pass @comps cfg
        mcomp      = mapLeft (wrap . pure)
                   . lookupComp tgtComp $ cfg ^. components
        tgtComp    = Component.tagRep @comp
        procComp i = (encoders .) <$> layerEncoder where
            memEncoder   = encodePassDataElem  @compMemPool
                         $ coerce (i ^. memPool)
            sizeEncoder  = encodePassDataElem  @compSize
                         $ ByteSize (i ^. byteSize)
            layerEncoder = encodePassDataElems @targets <$> layerOffsets
            initEncoder  = encodePassDataElem  @layerInit
                         $ Layer.DynamicManager @comp
                           (i ^. layersInitializer)
                           (i ^. layersConstructor)
                           (i ^. layersDestructor)
            travEncoder  = encodePassDataElem @compTravsl
                         $ Component.DynamicTraversal @comp (i ^. layersComponents)
            layerTypes   = someTypeReps @layers
            layerOffsets = view byteOffset <<$>> layerInfos
            layerInfos   = mapLeft wrap $ catEithers
                         $ flip lookupLayer (i ^. layers) <$> layerTypes
            encoders     = travEncoder
                         . initEncoder
                         . memEncoder
                         . sizeEncoder

    {-# INLINE encode__ #-}

lookupComp :: Component.TagRep -> Map Component.TagRep v -> EncodingResult v
lookupComp k m = justErr (MissingComponent k) $ Map.lookup k m ; {-# INLINE lookupComp #-}

lookupLayer :: SomeTypeRep -> Map SomeTypeRep v -> EncodingResult v
lookupLayer k m = justErr (MissingLayer k) $ Map.lookup k m ; {-# INLINE lookupLayer #-}

catEithers :: [Either l r] -> Either (NonEmpty l) [r]
catEithers lst = case partitionEithers lst of
    ([]    ,rs) -> Right rs
    ((l:ls),_)  -> Left $ l :| ls
{-# INLINE catEithers #-}

-- TODO
-- We should probably think about using other structure than Either here
-- Data.Validation is almost what we need, but its Semigroup instance
-- uses only lefts, which makes is not suitable for the purpose of
-- simple Either replacement.
appSemiLeft :: Semigroup e
            => (Either e (b -> c)) -> Either e (a -> b) -> Either e (a -> c)
appSemiLeft f a = case f of
    Left e -> case a of
        Left e' -> Left (e <> e')
        _       -> Left e
    Right ff -> case a of
        Left e   -> Left e
        Right aa -> Right $ ff . aa
{-# INLINE appSemiLeft #-}


-- === Attr encoder === --

encodeAttrs :: ∀ pass. AttrEncoder pass
            => [Any] -> (Pass.State pass -> Pass.State pass)
encodeAttrs = encodeAttrs__ @pass @(AttrEncoderTarget pass) ; {-# INLINE encodeAttrs #-}

type  AttrEncoder       pass = AttrEncoder__ pass (AttrEncoderTarget pass)
type  AttrEncoderTarget pass = Pass.Vars pass Pass.Attrs
class AttrEncoder__     pass (attrs :: [Type]) where
    encodeAttrs__ :: [Any] -> (Pass.State pass -> Pass.State pass)

instance AttrEncoder__ pass '[] where
    encodeAttrs__ _ = id ; {-# INLINE encodeAttrs__ #-}

instance ( attr ~ Attr a
         , AttrEncoder__ pass as
         , PassDataElemEncoder attr pass
         , Typeable a
         ) => AttrEncoder__ pass (a ': as) where
    encodeAttrs__ []     = impossible
    encodeAttrs__ (a:as) = encoder . subEncoder where
        encoder    = encodePassDataElem @attr (unsafeCoerce a :: attr)
        subEncoder = encodeAttrs__ @pass @as as
    {-# INLINE encodeAttrs__ #-}


-- === Attr encoder === --

decodeAttrs    :: ∀ pass. AttrDecoder    pass => Pass.State pass -> [Any]
decodeOutAttrs :: ∀ pass. OutAttrDecoder pass => Pass.State pass -> [Any]
decodeAttrs    = decodeAttrs__ @pass @(AttrDecoderTarget    pass) ; {-# INLINE decodeAttrs #-}
decodeOutAttrs = decodeAttrs__ @pass @(OutAttrDecoderTarget pass) ; {-# INLINE decodeOutAttrs #-}

type  AttrDecoder          pass = AttrDecoder__ pass (AttrDecoderTarget    pass)
type  OutAttrDecoder       pass = AttrDecoder__ pass (OutAttrDecoderTarget pass)
type  AttrDecoderTarget    pass = Pass.Vars pass Pass.Attrs
type  OutAttrDecoderTarget pass = Pass.Outs pass Pass.Attrs
class AttrDecoder__        pass (attrs :: [Type]) where
    decodeAttrs__ :: Pass.State pass -> [Any]

instance AttrDecoder__ pass '[] where
    decodeAttrs__ _ = mempty ; {-# INLINE decodeAttrs__ #-}

instance ( attr ~ Attr a
         , AttrDecoder__ pass as
         , PassDataElemDecoder attr pass
         , Typeable a
         ) => AttrDecoder__ pass (a ': as) where
    decodeAttrs__ s = a : as where
        a  = unsafeCoerce $ decodePassDataElem @attr s
        as = decodeAttrs__ @pass @as s
    {-# INLINE decodeAttrs__ #-}


-- === Element encoders === --

type  PassDataElemEncoder  el pass = PassDataElemsEncoder '[el] el pass
class PassDataElemsEncoder (els :: [Type]) t pass where
    encodePassDataElems :: [t] -> Pass.State pass -> Pass.State pass

encodePassDataElem :: ∀ t pass. PassDataElemEncoder t pass
                   => t -> Pass.State pass -> Pass.State pass
encodePassDataElem = encodePassDataElems @'[t] @t @pass . pure

instance PassDataElemsEncoder '[Imp] t   pass where encodePassDataElems = imp
instance PassDataElemsEncoder els    Imp pass where encodePassDataElems = imp
instance PassDataElemsEncoder els    t   Imp  where encodePassDataElems = imp
instance TypeMap.SetElemsFromList els t (Pass.StateLayout pass)
      => PassDataElemsEncoder els t pass where
    encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals ; {-# INLINE encodePassDataElems #-}


type PassDataElemDecoder t pass = TypeMap.ElemGetter t (Pass.StateLayout pass)

decodePassDataElem :: ∀ t pass. PassDataElemDecoder t pass
                   => Pass.State pass -> t
decodePassDataElem = TypeMap.getElem @t . unwrap ; {-# INLINE decodePassDataElem #-}
