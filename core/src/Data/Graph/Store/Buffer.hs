{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Buffer where

import Prologue hiding (Data, pprint, print, putStrLn)

import qualified Control.Monad.State.Layered           as State
import qualified Data.ByteString.Internal              as ByteString
import qualified Data.Convert2                         as Convert
import qualified Data.Convert2                         as Convert
import qualified Data.Generics.Traversable             as GTraversable
import qualified Data.Graph.Component.Edge.Class       as Edge
import qualified Data.Graph.Component.Node.Class       as Node
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Component.Maybe       as MaybeComponent
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Class           as Layer
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Fold.LayerMap              as LayerMap
import qualified Data.Graph.Fold.Partition             as Partition
import qualified Data.Graph.Store.Size.Class           as Size
import qualified Data.Map.Strict                       as Map
import qualified Data.Mutable.Class                    as Mutable
import qualified Data.Mutable.Class2                   as Mutable
import qualified Data.Mutable.Plain                    as Data
import qualified Data.Mutable.Storable.Array           as Array
import qualified Data.Mutable.Storable.SmallAutoVector as SAV
import qualified Data.Set                              as Set
import qualified Data.Storable                         as Struct
import qualified Foreign.ForeignPtr                    as ForeignPtr
import qualified Foreign.Memory.Pool                   as MemPool
import qualified Foreign.Storable                      as StdStorable
import qualified Foreign.Storable.Class                as Storable
import qualified Luna.IR                               as IR
import qualified Memory                                as Memory
import qualified Type.Data.List                        as List

import qualified Data.Graph.Fold.Class     as Fold
import qualified Data.Graph.Fold.Filter    as Fold
import qualified Data.Graph.Fold.Scoped    as Fold
import qualified Data.Graph.Fold.ScopedMap as Fold
import qualified Data.Graph.Fold.Struct    as Fold

import Data.ByteString                       (ByteString)
import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.Maybe       (MaybeComponent)
import Data.Graph.Data.Component.Set         (ComponentSet, ComponentSetA)
import Data.Graph.Data.Component.Vector      (ComponentVector, ComponentVectorA)
import Data.Graph.Store.Size.Class           (Size)
import Data.Map.Strict                       (Map)
import Data.Mutable.Storable.Array           (ManagedArray)
import Data.Mutable.Storable.SmallAutoVector (SmallVectorA)
import Data.Set                              (Set)
import Data.Storable                         (type (-::), ManagedStruct)
import Foreign.ForeignPtr                    (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe             (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Utils              (SomeForeignPtr)
import Foreign.Memory.Pool                   (MemPool)
import Foreign.Ptr                           (Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Semigroup                   (type (<>))

import qualified Luna.IR.Term             as IR
import qualified Luna.IR.Term.Ast.Invalid as InvalidIR
import qualified Type.Known               as Type
import qualified Type.Show                as Type


type RedirectMap = Map Memory.SomeUnmanagedPtr Memory.SomeUnmanagedPtr


putStrLn :: Applicative m => String -> m ()
putStrLn = const $ pure ()

print :: Applicative m => a -> m ()
print = const $ pure ()

pprint :: Applicative m => a -> m ()
pprint = const $ pure ()

------------------------------
-- === DynamicMemRegion === --
------------------------------

-- === Definition === --

newtype UnknownSizeMemRegion t a = UnknownSizeMemRegion (Memory.Ptr t a)
makeLenses ''UnknownSizeMemRegion

unsafeNull :: Memory.PtrType t => UnknownSizeMemRegion t a
unsafeNull = UnknownSizeMemRegion Memory.nullPtr
{-# INLINE unsafeNull #-}

-- FIXME: change it to Struct.FieldInitializer
instance Applicative m
      => Storable.Poke Struct.Fieldx m (UnknownSizeMemRegion t a) where
    poke = \_ _ -> pure ()
    {-# INLINE poke #-}


type instance Item (UnknownSizeMemRegion t a) = a

deriving instance Show (Memory.Ptr t a) => Show (UnknownSizeMemRegion t a)

instance (Memory.PtrType t, Storable.Peek Storable.View m a, MonadIO m, Storable.KnownConstantSize a)
      => Mutable.Read m (UnknownSizeMemRegion t a) where
    unsafeRead = \a ix -> Memory.withUnmanagedPtr (unwrap a)
        $ \p -> Storable.peekElemOff @Storable.View p ix
    {-# INLINE unsafeRead #-}

instance (Memory.PtrType t, Storable.Poke Storable.View m a, MonadIO m, Storable.KnownConstantSize a)
      => Mutable.Write m (UnknownSizeMemRegion t a) where
    unsafeWrite = \a ix v -> Memory.withUnmanagedPtr (unwrap a)
        $ \p -> Storable.pokeElemOff @Storable.View p ix v
    {-# INLINE unsafeWrite #-}











-------------------------------
-- === StoreDynAllocator === --
-------------------------------

-- === Definition === --

data StoreDyn
type StoreDynAllocator = 'Memory.Allocator StoreDyn

newtype StoreDynState = StoreDynState (Memory.UnmanagedPtr ())
makeLenses ''StoreDynState

instance (MonadIO m, State.Monad StoreDynState m, Storable.KnownConstantSize a)
      => Memory.Allocation StoreDynAllocator 'Memory.Unmanaged a m where
    allocate n = do
        ptr <- unwrap <$> State.get @StoreDynState
        let byteSize = n * Storable.constantSize @a
        State.put @StoreDynState $ wrap $ ptr `Memory.plus` byteSize
        putStrLn $ "allocate " <> show n <> " " <> show byteSize <> " = " <> show ptr
        pure (coerce ptr)


--------------------------------
-- === CopyInitialization === --
--------------------------------

-- === Definition === --

data CopyInitialization
type instance Fold.Result     CopyInitialization = ()
type instance Fold.LayerScope CopyInitialization = 'Fold.All

instance Monad m => Fold.ComponentBuilder CopyInitialization m comp

instance (MonadIO m, Type.Show layer, CopyInitializerP1 m (Layer.Cons layer))
      => Fold.LayerBuilder CopyInitialization m layer where
        layerBuild = \layer m -> do
            putStrLn $ "CopyInitializer layer " <> Type.show @layer -- <> "!!"
            m <* copyInitializeP1 layer


-- === CopyInitializerP1 === --

class Applicative m => CopyInitializerP1 m a where
    copyInitializeP1 :: ∀ t1. a t1 -> m (a t1)
    copyInitializeP1 = pure
    {-# INLINE copyInitializeP1 #-}

instance (MonadIO m, Data.CopyInitializer1 m (ComponentSetA StoreDynAllocator comp), Applicative m)
      => CopyInitializerP1 m (ComponentSet comp) where
    copyInitializeP1 = \a -> do
        -- a <$ Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
        print "!!! CopyInitializer (ComponentSet comp)"
        a1 <- Mutable.toList a
        print a1
        Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
        a2 <- Mutable.toList a
        print a2
        when_ (a1 /= a2) $ do
            putStrLn "COPY INITIALIZER ERROR"
            print a1
            print a2
            error "COPY INITIALIZER ERROR"
        pure a

instance Applicative m => CopyInitializerP1 m (Component comp)

instance {-# OVERLAPPABLE #-}
    ( Fold.Builder1 (Fold.Struct CopyInitialization) m a
    , Applicative m
    ) => CopyInitializerP1 m a where
    copyInitializeP1 = \a -> a <$ (Fold.build1 @(Fold.Struct CopyInitialization) a (pure ()))


-- === CopyInitialization Folds === --

instance ( MonadIO m -- debug
      , Data.CopyInitializer1 m (ComponentSetA StoreDynAllocator comp), Monad m)
      => Fold.Builder1 CopyInitialization m (ComponentSet comp) where
    build1 = \a x -> do
        -- x <* Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
        print "! CopyInitializer (ComponentSet comp)"
        print a
        Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
        print a
        x
    {-# INLINE build1 #-}

instance (MonadIO m -- debug
    , Data.CopyInitializer1 m (ComponentVectorA StoreDynAllocator comp), Monad m)
      => Fold.Builder1 CopyInitialization m (ComponentVector comp) where
    build1 = \a x -> do
        print "? CopyInitializer (ComponentVector comp)"
        x <* Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
    {-# INLINE build1 #-}

instance (MonadIO m -- debug
      , Data.CopyInitializer m (SmallVectorA t StoreDynAllocator comp a), Monad m)
      => Fold.Builder CopyInitialization m (SmallVectorA t alloc comp a) where
    build = \a x -> do
        print "? CopyInitializer (SmallVectorA ...)"
        x <* Data.copyInitialize (Memory.setAllocator @StoreDynAllocator a)
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} (Monad m, Fold.Builder CopyInitialization m a)
      => Fold.Builder CopyInitialization m (Maybe a) where
    build = \m x -> maybe x (\t -> Fold.build @CopyInitialization t x) m
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-}
    ( Monad m, Fold.Builder CopyInitialization m a
    , Fold.Builder CopyInitialization m b
    ) => Fold.Builder CopyInitialization m (a, b) where
    build = \(a, b) x -> Fold.build @CopyInitialization a
                       $ Fold.build @CopyInitialization b x
    {-# INLINE build #-}

instance Monad m => Fold.Builder1 CopyInitialization m (Component comp)



-----------------------------
-- === CopyInitializer === --
-----------------------------

-- === Definition === --

type ComponentStaticInitializer m =
    ( ComponentStaticInitializer__ (Graph.ComponentsM m) (State.StateT StoreDynState m)
    , Graph.KnownComponentNumber (Graph.Discover m)
    , MonadIO m
    )

copyInitializeComponents :: ∀ m. ComponentStaticInitializer m
    => [Int] -> BufferM m -> m ()
copyInitializeComponents ccount buffer = do
    let dataReg  = dataRegion buffer
    dynDataReg  <- dynDataRegion buffer
    Memory.withUnmanagedPtr (unwrap dynDataReg) $ \ptr
        -> flip (State.evalT @StoreDynState) (StoreDynState ptr)
         $ copyInitializeComponents__ @(Graph.ComponentsM m) ccount dataReg
{-# INLINE copyInitializeComponents #-}



-- === ComponentInitializerFold === --

type ComponentInitializerFold m comp
   = Fold.Builder1 (Fold.Scoped CopyInitialization) m (Component comp)

foldInitializeComponent :: ComponentInitializerFold m comp => Component comp layout -> m ()
foldInitializeComponent = \comp -> Fold.build1 @(Fold.Scoped CopyInitialization) comp (pure ())

class ComponentStaticInitializer__ (comps :: [Type]) m where
    copyInitializeComponents__ :: [Int] -> BufferDataRegion -> m ()

instance Applicative m
      => ComponentStaticInitializer__ '[] m where
    copyInitializeComponents__ = \_ _ -> pure ()
    {-# INLINE copyInitializeComponents__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentInitializerFold m comp
    , ComponentStaticInitializer__ comps m
    , MonadIO m
    -- debug:
    , Type.Show comp
    )
      => ComponentStaticInitializer__ (comp ': comps) m where
    copyInitializeComponents__ = \(compCount : compCounts) region -> do
        let compSize   = Layer.byteSize @layers
            regionPtr  = unwrap region
            regionPtr' = wrap $ regionPtr `Memory.plus` (compSize * compCount)
        putStrLn ""
        putStrLn $ "copyInitializeComponents for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ "\n>> " <> show ix <> " (" <> show region <> ")"
            let ptr  = regionPtr `Memory.plus` (ix * compSize)
            putStrLn $ "??? " <> show ptr
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldInitializeComponent comp

        copyInitializeComponents__ @comps compCounts regionPtr'



---------------------------------
-- === Pointer redirection === --
---------------------------------


type ComponentStaticRedirection comps m
    = ( ComponentStaticRedirection__ comps (State.StateT RedirectMap m)
      , Monad m
      )
redirectComponents :: ∀ comps m. ComponentStaticRedirection comps m
    => RedirectMap -> [Int] -> BufferDataRegion -> m ()
redirectComponents = \m ccount region -> flip (State.evalT @RedirectMap) m
    $ redirectComponents__ @comps ccount region


class ComponentStaticRedirection__ (comps :: [Type]) m where
    redirectComponents__ :: [Int] -> BufferDataRegion -> m ()

instance Applicative m
      => ComponentStaticRedirection__ '[] m where
    redirectComponents__ = \_ _ -> pure ()
    {-# INLINE redirectComponents__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentRedirectFold m comp
    , ComponentStaticRedirection__ comps m
    , MonadIO m
    , FoldRedirectComponent comp m
    -- debug:
    , Type.Show comp
    )
      => ComponentStaticRedirection__ (comp ': comps) m where
    redirectComponents__ = \(compCount : compCounts) region -> do
        let compSize   = Layer.byteSize @layers
            regionPtr  = unwrap region
            regionPtr' = wrap $ regionPtr `Memory.plus` (compSize * compCount)
        putStrLn $ "redirectComponents for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ ">> " <> show ix <> " (" <> show region <> ")"
            let ptr  = regionPtr `Memory.plus` (ix * compSize)
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldRedirectComponent comp

        redirectComponents__ @comps compCounts regionPtr'

data ComponentRedirection
type instance Fold.Result     ComponentRedirection = ()
type instance Fold.LayerScope ComponentRedirection = 'Fold.All

instance Monad m => Fold.ComponentMap ComponentRedirection m comp

instance {-# OVERLAPPABLE #-}
    (Show (Layer.Cons layer ()), Type.Show layer, MonadIO m, PointerRedirection1 m (Layer.Cons layer), State.Getter RedirectMap m)
      => Fold.LayerMap ComponentRedirection m layer where
        mapLayer = \a _ -> do
            print $ "redirecting layer " <> Type.show @layer
            print (unsafeCoerce a :: Layer.Cons layer ())
            m <- State.get @RedirectMap
            let f ptr = do
                    putStrLn $ "LOOKUP " <> show ptr
                    let mitem = Map.lookup ptr m
                    case mitem of
                        Just x  -> pure x
                        Nothing -> do
                            error "REDIRECTION LOOKUP ERROR"
                    -- pure . unsafeFromJust . flip Map.lookup m $ ptr
            (,()) <$> redirectPointers1 f a

            -- Fold.build1 @ComponentRedirection

instance {-# OVERLAPPABLE #-}
    (MonadIO m, PointerRedirection1 m (Layer.Cons layer), State.Getter RedirectMap m, layer ~ IR.Users)
      => Fold.LayerMap ComponentRedirection m IR.Users where
        mapLayer = \a _ -> do
            print $ "redirecting layerx " <> Type.show @layer
            lst <- Mutable.toList a
            print lst
            m <- State.get @RedirectMap
            let f ptr = do
                    putStrLn $ "LOOKUP " <> show ptr
                    let mitem = Map.lookup ptr m
                    case mitem of
                        Just x  -> pure x
                        Nothing -> do
                            lst <- Mutable.toList a
                            error $ "REDIRECTION LOOKUP ERROR. Layer = "
                               <> Type.show @layer
                               <> ". Key = " <> show ptr
                               <> " " <> show a
                    -- pure . unsafeFromJust . flip Map.lookup m $ ptr
            (,()) <$> redirectPointers1 f a

            -- Fold.build1 @ComponentRedirection




type ComponentRedirectFold m comp
   = ( MonadIO m, Type.Show comp -- debug
    --  , Layer.Reader (Component comp) IR.Model m -- debug
     , Fold.Builder1 (Fold.ScopedMap ComponentRedirection) m (Component comp)
     )

class FoldRedirectComponent comp m where
    foldRedirectComponent :: forall layout. ComponentRedirectFold m comp => Component comp layout -> m ()

instance {-# OVERLAPPABLE #-} FoldRedirectComponent comp m where
    foldRedirectComponent = \comp -> do
        putStrLn $ "Redirecting component " <> Type.show @comp -- <> convert (IR.showTag comp)
        -- print . IR.showTag =<< Layer.read @IR.Model comp
        Fold.build1 @(Fold.ScopedMap ComponentRedirection) comp (pure ())


instance Layer.Reader IR.Term IR.Model m
      => FoldRedirectComponent IR.Terms m where
    foldRedirectComponent = \comp -> do
        putStrLn $ "\nRedirecting Term " <> show comp -- <> convert (IR.showTag comp)
        print . IR.showTag =<< Layer.read @IR.Model comp
        Fold.build1 @(Fold.ScopedMap ComponentRedirection) comp (pure ())


class PointerRedirection1 m a where
    redirectPointers1 :: (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
                      -> a t1 -> m (a t1)
    default redirectPointers1 :: Applicative m
                              => (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
                              -> a t1 -> m (a t1)
    redirectPointers1 = \_ -> pure

class PointerRedirection m a where
    redirectPointers :: (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
                     -> a -> m a

    default redirectPointers :: Applicative m => (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr) -> a -> m a
    redirectPointers = \_ -> pure

instance MonadIO m
      => PointerRedirection1 m (ComponentSetA alloc tag) where
    redirectPointers1 = \f a -> do
        putStrLn ">> redirection ComponentSetA"
        out <- a <$ Mutable.mapM a (redirectPointers1 f)
        putStrLn ">> redirection ComponentSetA"
        pure out
    {-# INLINE redirectPointers1 #-}

instance Applicative m => PointerRedirection1 m (Component comp) where
    redirectPointers1 = \f a -> Component.unsafeFromPtr
        . unwrap <$> f (wrap $ Component.unsafeToPtr a)

instance Applicative m => PointerRedirection1 m (Layer.Simple a)

instance Applicative m => PointerRedirection1 m (MaybeComponent comp) where
    redirectPointers1 = MaybeComponent.mapM . redirectPointers1
    {-# INLINE redirectPointers1 #-}

instance MonadIO m => PointerRedirection m (ComponentVectorA alloc tag layout) where
    redirectPointers = \f a -> do
        putStrLn ">> redirection ComponentVectorA"
        out <- a <$ Mutable.mapM a (redirectPointers1 f)
        putStrLn "<< redirection ComponentVectorA"
        pure out
    {-# INLINE redirectPointers #-}

instance Applicative m => PointerRedirection m (Component comp layout) where
    redirectPointers = redirectPointers1

instance Applicative m => PointerRedirection m Int
instance Applicative m => PointerRedirection m Word8
instance Applicative m => PointerRedirection m Word16
instance Applicative m => PointerRedirection m Word32
instance Applicative m => PointerRedirection m Word64
instance Applicative m => PointerRedirection m Bool

instance
    ( ctx ~ PointerRedirection m
    , MonadIO m
    ) => PointerRedirection1 m IR.UniTerm where
    redirectPointers1 = \f
        -> GTraversable.gmapM @(GTraversable.GTraversable ctx)
         $ GTraversable.gmapM @ctx (redirectPointers f)

instance Applicative m => PointerRedirection m (SmallVectorA t alloc n IR.Name)
instance Applicative m => PointerRedirection m (SmallVectorA t alloc n IR.Qualified)
instance Applicative m => PointerRedirection m (SmallVectorA t alloc n Char)
instance Applicative m => PointerRedirection m (SmallVectorA t alloc n Word8)
instance Applicative m => PointerRedirection m IR.Name
instance Applicative m => PointerRedirection m IR.Qualified
instance Applicative m => PointerRedirection m IR.ForeignImportType
instance Applicative m => PointerRedirection m IR.ImportSourceData
instance Applicative m => PointerRedirection m IR.ImportTargetData
instance Applicative m => PointerRedirection m InvalidIR.Symbol




-------------------------------
-- === Pointer swizzling === --
-------------------------------


-- type ComponentStaticRedirection comps m
--     = ( ComponentUnswizzle__ comps (State.StateT RedirectMap m)
--       , Monad m
--       )
-- redirectComponents :: ∀ comps m. ComponentStaticRedirection comps m
--     => RedirectMap -> [Int] -> BufferDataRegion -> m ()
-- redirectComponents = \m ccount region -> flip (State.evalT @RedirectMap) m
--     $ unswizzleComponents__ @comps ccount region


class ComponentUnswizzle__ (comps :: [Type]) m where
    unswizzleComponents__ :: [Int] -> BufferDataRegion -> m ()

instance Applicative m
      => ComponentUnswizzle__ '[] m where
    unswizzleComponents__ = \_ _ -> pure ()
    {-# INLINE unswizzleComponents__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentUnswizzlingFold m comp
    , ComponentUnswizzle__ comps m
    , MonadIO m
    -- debug:
    , Type.Show comp
    )
      => ComponentUnswizzle__ (comp ': comps) m where
    unswizzleComponents__ = \(compCount : compCounts) region -> do
        let compSize   = Layer.byteSize @layers
            regionPtr  = unwrap region
            regionPtr' = wrap $ regionPtr `Memory.plus` (compSize * compCount)
        putStrLn $ "unswizzleComponents for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ ">> " <> show ix <> " (" <> show region <> ")"
            let ptr  = regionPtr `Memory.plus` (ix * compSize)
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldUnswizzleComponents comp

        unswizzleComponents__ @comps compCounts regionPtr'

data ComponentUnswizzling
type instance Fold.Result     ComponentUnswizzling = ()
type instance LayerMap.LayerScope ComponentUnswizzling = 'LayerMap.All

-- newtype LayerPtr1 layer = LayerPtr1 (forall layout. Ptr (Layer.Cons layer layout))

-- instance (MonadIO m, Mutable.Unswizzle m (SAV.Field1 (Layer.Cons layer)))
--       => LayerMap.LayerMap ComponentUnswizzling m layer where
--         mapLayerPtr = \a _ -> () <$ Mutable.unswizzle (SAV.Field1 $ wrap a)

instance (MonadIO m, UnswizzleX (Layer.Cons layer) m)
      => LayerMap.LayerMap ComponentUnswizzling m layer where
        mapLayerPtr = \a _ -> () <$ unswizzleX a

class UnswizzleX a m where
    unswizzleX :: forall t1. Ptr (a t1) -> m ()
    default unswizzleX :: Applicative m => Ptr (a t1) -> m ()
    unswizzleX = \_ -> pure () ; {-# INLINE unswizzleX #-}

instance MonadIO m => UnswizzleX (ComponentSetA alloc tag) m where
    unswizzleX = Mutable.unswizzle1 <=< (liftIO . StdStorable.peek)

instance MonadIO m => UnswizzleX (MaybeComponent comp) m where
    unswizzleX = \p -> do
        may        <- liftIO $ StdStorable.peek p
        unswizzled <- MaybeComponent.mapM (Mutable.unswizzleRelTo $ castPtr p) may
        liftIO $ StdStorable.poke p unswizzled
    {-# INLINE unswizzleX #-}


instance MonadIO m => UnswizzleX (Component comp) m where
    unswizzleX = Mutable.unswizzle . SAV.Field . wrap

instance Applicative m => UnswizzleX (Layer.Simple a) m

            -- Fold.build1 @ComponentUnswizzling

instance
    ( ctx ~ Mutable.UnswizzleRelTo m
    , MonadIO m
    ) => UnswizzleX IR.UniTerm m where
    unswizzleX = \ptr -> do
        putStrLn ">> uni unswizzle"
        uni <- liftIO $ StdStorable.peek ptr
        GTraversable.gmapM @(GTraversable.GTraversable ctx)
            (GTraversable.gmapM @ctx (Mutable.unswizzleRelTo (coerce ptr))) uni
        putStrLn "<< uni unswizzle"
        pure ()

instance MonadIO m => Mutable.UnswizzleRelTo m (ComponentVectorA alloc tag layout) where
    unswizzleRelTo = \_ a -> a <$ Mutable.unswizzle1 a

instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n IR.Name) where
    unswizzleRelTo = \_ a -> pure a

instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n IR.Qualified) where
    unswizzleRelTo = \_ a -> pure a

instance Applicative m => Mutable.UnswizzleRelTo m (Component comp layout) where
    unswizzleRelTo = \ptr comp -> do
        let compPtr = Component.unsafeToPtr comp
        pure $ Component.unsafeFromPtr (unsafeCoerce $ compPtr `minusPtr` ptr)

instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n Char)   where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n Int)    where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n Word8)  where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n Word16) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n Word32) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (SmallVectorA t alloc n Word64) where unswizzleRelTo = \_ a -> pure a


instance Applicative m => Mutable.UnswizzleRelTo m (Char)   where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (Int)    where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (Word8)  where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (Word16) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (Word32) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (Word64) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (IR.Name) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (IR.Qualified) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (IR.ForeignImportType) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (Bool) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (InvalidIR.Symbol) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (IR.ImportSourceData) where unswizzleRelTo = \_ a -> pure a
instance Applicative m => Mutable.UnswizzleRelTo m (IR.ImportTargetData) where unswizzleRelTo = \_ a -> pure a

    -- instance
--     ( ctx ~ Buffer.PointerRedirection m
--     , MonadIO m
--     ) => Buffer.PointerRedirection1 m IR.UniTerm where
--     redirectPointers1 = \f
--         -> GTraversable.gmapM @(GTraversable.GTraversable ctx)
--          $ GTraversable.gmapM @ctx (Buffer.redirectPointers f)

instance MonadIO m
      => Mutable.UnswizzleP1 m (ComponentSetA alloc tag) where
    unswizzleP1 = \a -> a <$ Mutable.unswizzle1 a


type ComponentUnswizzlingFold m comp
   = Fold.Builder1 (LayerMap.Scoped ComponentUnswizzling) m (Component comp)

foldUnswizzleComponents :: ComponentUnswizzlingFold m comp => Component comp layout -> m ()
foldUnswizzleComponents = \comp -> Fold.build1 @(LayerMap.Scoped ComponentUnswizzling) comp (pure ())

-- class PointerRedirection1 m a where
--     redirectPointers1 :: (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
--                       -> a t1 -> m (a t1)

-- class PointerRedirection m a where
--     redirectPointers :: (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
--                      -> a -> m a

--     default redirectPointers :: Applicative m => (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr) -> a -> m a
--     redirectPointers = \_ -> pure

-- instance MonadIO m
--       => PointerRedirection1 m (ComponentSetA alloc tag) where
--     redirectPointers1 = \f a -> do
--         putStrLn ">> redirection ComponentSetA"
--         out <- a <$ Mutable.mapM a (redirectPointers1 f)
--         putStrLn ">> redirection ComponentSetA"
--         pure out
--     {-# INLINE redirectPointers1 #-}

-- instance Applicative m => PointerRedirection1 m (Component comp) where
--     redirectPointers1 = \f a -> Component.unsafeFromPtr
--         . unwrap <$> f (wrap $ Component.unsafeToPtr a)


-- instance MonadIO m => PointerRedirection m (ComponentVectorA alloc tag layout) where
--     redirectPointers = \f a -> do
--         putStrLn ">> redirection ComponentVectorA"
--         out <- a <$ Mutable.mapM a (redirectPointers1 f)
--         putStrLn "<< redirection ComponentVectorA"
--         pure out
--     {-# INLINE redirectPointers #-}

-- instance Applicative m => PointerRedirection m (Component comp layout) where
--     redirectPointers = redirectPointers1

-- instance Applicative m => PointerRedirection m Word8
-- instance Applicative m => PointerRedirection m Word16
-- instance Applicative m => PointerRedirection m Word32
-- instance Applicative m => PointerRedirection m Word64
-- instance Applicative m => PointerRedirection m Bool



instance MonadIO m
      => Mutable.Unswizzle m (SAV.Field (Component tag layout)) where
    unswizzle = \(SAV.Field ptr) -> do
        comp <- liftIO $ StdStorable.peek (unwrap ptr) -- fiix storable class ptr handling
        let compPtr = Component.unsafeToPtr comp
        liftIO $ StdStorable.poke (coerce $ unwrap ptr) (compPtr `minusPtr` (unwrap ptr))


--------------------------
-- === MemoryRegion === --
--------------------------

-- === Definition === --

newtype Buffer graph = Buffer (ManagedStruct (BufferLayout graph))

type HeaderLayout graph =
   '[ "staticDataRegionSize"  -:: Int
    , "dynamicDataRegionSize" -:: Int
    , "pointerDataRegionSize" -:: Int
    , "componentElems"        -:: ManagedArray (Graph.ComponentNumber graph) Int
    ]

type BufferDataRegion = UnknownSizeMemRegion 'Memory.Managed ()

type BufferLayout graph =
    HeaderLayout graph <>
   '[ "memoryRegion" -:: BufferDataRegion
    ]

instance Struct.IsStruct (Buffer graph)
type instance Memory.Management (Buffer graph) = 'Memory.Managed


-- === Aliases === --

type BufferM       m = Buffer       (Graph.Discover m)
type HeaderLayoutM m = HeaderLayout (Graph.Discover m)


-- === Fields === --

staticDataRegionSize  :: Struct.Lens "staticDataRegionSize"
dynamicDataRegionSize :: Struct.Lens "dynamicDataRegionSize"
pointerDataRegionSize :: Struct.Lens "pointerDataRegionSize"
field_componentElems  :: Struct.Lens "componentElems"
field_memoryRegion    :: Struct.Lens "memoryRegion"
staticDataRegionSize  = Struct.autoLens ; {-# INLINE staticDataRegionSize  #-}
dynamicDataRegionSize = Struct.autoLens ; {-# INLINE dynamicDataRegionSize #-}
pointerDataRegionSize = Struct.autoLens ; {-# INLINE pointerDataRegionSize #-}
field_componentElems  = Struct.autoLens ; {-# INLINE field_componentElems  #-}
field_memoryRegion    = Struct.autoLens ; {-# INLINE field_memoryRegion    #-}


dataRegion :: Graph.KnownComponentNumber graph
    => Buffer graph -> BufferDataRegion
dataRegion = coerce . unwrap . Struct.ref field_memoryRegion
{-# INLINE dataRegion #-}

dynDataRegion :: (Graph.KnownComponentNumber graph, MonadIO m)
    => Buffer graph -> m BufferDataRegion
dynDataRegion = \a -> do
    staticSize <- Struct.read staticDataRegionSize  a
    pure $ coerce (unwrap (Struct.ref field_memoryRegion a) `Memory.plus` staticSize)
{-# INLINE dynDataRegion #-}

componentElems :: Buffer graph -> ManagedArray (Graph.ComponentNumber graph) Int
componentElems = coerce . unwrap . Struct.ref field_componentElems
{-# INLINE componentElems #-}


-- === API === --

type Alloc m = (MonadIO m, Graph.KnownComponentNumberM m)

alloc :: ∀ m. Alloc m => [Int] -> Size -> m (BufferM m)
alloc = \ccount size -> liftIO $ do
    let headerSize = Storable.constantSize @(HeaderLayoutM m)
        bodySize   = Size.total size
        totalSize  = headerSize + bodySize
        staticRegionSize = size ^. Size.static
        dataRegionSize   = size ^. (Size.dynamic . Size.dataRegion)
        ptrRegionSize    = size ^. (Size.dynamic . Size.ptrRegion)

    ptr <- Memory.mallocBytes totalSize
    putStrLn $ "Buffer malloc (" <> show totalSize <> " bytes) = " <> show ptr

    let struct   = Struct.unsafeCastFromPtr ptr
        elsCount = componentElems struct

    Struct.write staticDataRegionSize  struct staticRegionSize
    Struct.write dynamicDataRegionSize struct dataRegionSize
    Struct.write pointerDataRegionSize struct ptrRegionSize
    Mutable.unsafeWriteFromList elsCount ccount

    pure struct


-- === Conversions === --

unsafeFreeze :: ∀ m graph. (MonadIO m, Graph.KnownComponentNumberM m)
    => Buffer graph -> m ByteString
unsafeFreeze = \a -> do
    let headerSize = Storable.constantSize @(HeaderLayoutM m)
    staticSize  <- Struct.read staticDataRegionSize  a
    dynDataSize <- Struct.read dynamicDataRegionSize a
    dynPtrSize  <- Struct.read pointerDataRegionSize a
    let totalSize = headerSize + staticSize + dynDataSize + dynPtrSize
    pure $ ByteString.PS (coerce a) 0 totalSize

unsafeThaw :: Monad m => ByteString -> m (Buffer (Graph.Discover m))
unsafeThaw = \(ByteString.PS ptr _ _) -> pure $ coerce ptr
{-# INLINE unsafeThaw #-}


-- === Instances === --

makeLenses ''Buffer
deriving instance Show (Buffer graph)



---------------------------------
-- === StaticRegionEncoder === --
---------------------------------

-- === Definition === --


type StaticRegionEncoder comps m
    = (StaticRegionEncoder__ comps (State.StateT RedirectMap m), Monad m)

encodeStaticRegion :: StaticRegionEncoder comps m
    => Node.Node layout
    -> Partition.Clusters comps -> Memory.SomeUnmanagedPtr -> m RedirectMap
encodeStaticRegion = flip State.execT mempty .:. (encodeStaticRegion__ . Layout.unsafeRelayout)


-- === Internal === --

class StaticRegionEncoder__ comps m where
    encodeStaticRegion__
        :: Node.Node ()
        -> Partition.Clusters comps
        -> Memory.SomeUnmanagedPtr
        -> m Memory.SomeUnmanagedPtr

instance Applicative m
      => StaticRegionEncoder__ '[] m where
    encodeStaticRegion__ = \_ _ -> pure
    {-# INLINE encodeStaticRegion__ #-}

instance {-# OVERLAPPABLE #-}
    ( Partition.SplitHead comp comps
    , StaticRegionEncoder__ comps m
    , StaticComponentEncoder__ comp m
    , Monad m
    ) => StaticRegionEncoder__ (comp ': comps) m where
    encodeStaticRegion__ = \root clusters ptr -> do
        let (!compSet, !clusters') = Partition.splitHead clusters
        ptr' <- setFoldlM encodeComponentStatic__ ptr compSet
        encodeStaticRegion__ root clusters' ptr'
    {-# INLINE encodeStaticRegion__ #-}

-- Sorting root component to be first one
instance
    ( Partition.SplitHead comp comps
    , StaticRegionEncoder__ comps m
    , StaticComponentEncoder__ comp m
    , Monad m
    , comp ~ Node.Nodes
    ) => StaticRegionEncoder__ (Node.Nodes ': comps) m where
    encodeStaticRegion__ = \root clusters ptr -> do
        let (!compSet, !clusters') = Partition.splitHead clusters
            compList = root : Set.toList (Set.delete root compSet)
        ptr' <- foldlM encodeComponentStatic__ ptr compList
        encodeStaticRegion__ root clusters' ptr'
    {-# INLINE encodeStaticRegion__ #-}


class StaticComponentEncoder__ comp m where
    encodeComponentStatic__
        :: Memory.SomeUnmanagedPtr
        -> Component.Some comp
        -> m Memory.SomeUnmanagedPtr

setFoldlM :: Monad m
       => (a -> t -> m a) -> a -> Set t -> m a
setFoldlM = \f z0 xs ->
    let f' x k z = f z x >>= k
    in  Set.foldr f' pure xs z0
{-# INLINE setFoldlM #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , State.Monad RedirectMap m
    , MonadIO m
    ) => StaticComponentEncoder__ comp m where
    encodeComponentStatic__ = \tgtPtr comp -> do
        let srcPtr   = Convert.convert (Component.unsafeToPtr comp)
            compSize = Layer.byteSize @layers
        putStrLn $ "copyAndOffsetBytes " <> show (tgtPtr, srcPtr, compSize)
        nextSrcPtr <- Memory.copyAndOffsetBytes tgtPtr srcPtr compSize
        State.modify_ @RedirectMap $ Map.insert srcPtr tgtPtr
        return nextSrcPtr
    {-# INLINE encodeComponentStatic__ #-}



---------------------------------------------
---------------------------------------------
---------------------------------------------
---------------------------------------------
---------------------------------------------


-----------------------------------
-- === ComponentCountDecoder === --
-----------------------------------

class ComponentCountDecoder m where
    decodeComponentCount :: BufferM m -> m [Int]

instance
    ( compNum ~ Graph.ComponentNumberM m
    , MonadIO m
    , Type.KnownInt compNum
    ) => ComponentCountDecoder m where
    decodeComponentCount = \buffer -> do
        let compNum = Type.val' @compNum :: Int
            elsArr = componentElems buffer
        putStrLn $ "Deserializing. Component type count: " <> show compNum

        ccount <- mapM (Mutable.unsafeRead elsArr) [0 .. compNum - 1]
        putStrLn $ "ccount: " <> show ccount
        pure ccount
    {-# INLINE decodeComponentCount #-}



-------------------------------------
-- === StaticComponentsDecoder === --
-------------------------------------


type DecodeOffsetMap = [Int]
type DecodeRegions   = [Memory.SomeUnmanagedPtr]

class StaticComponentsDecoder m where
    decodeStaticComponents :: [Int] -> BufferM m -> m (DecodeOffsetMap, DecodeRegions)

instance
    ( comps ~ Graph.ComponentsM m
    , StaticComponentsDecoder__ comps m
    , Graph.KnownComponentNumberM m
    , MonadIO m
    ) => StaticComponentsDecoder m where
    decodeStaticComponents = \ccount buffer -> do
        let dr            = dataRegion buffer
        let dataRegionPtr = unwrap dr
        Memory.withUnmanagedPtr dataRegionPtr $ decodeStaticComponents__ @comps ccount
    {-# INLINE decodeStaticComponents #-}

class StaticComponentsDecoder__ (comps :: [Type]) m where
    decodeStaticComponents__ :: [Int] -> Memory.SomeUnmanagedPtr -> m (DecodeOffsetMap, DecodeRegions)

instance Applicative m
      => StaticComponentsDecoder__ '[] m where
    decodeStaticComponents__ = \_ _ -> pure (mempty, mempty)
    {-# INLINE decodeStaticComponents__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , State.Getter (MemPool (Component comp ())) m
    , MonadIO m
    , StaticComponentsDecoder__ comps m
    , Type.Show comp
    ) => StaticComponentsDecoder__ (comp ': comps) m where
    decodeStaticComponents__ = \(n:ns) srcPtr -> do
        putStrLn $ "Static component decoder: " <> Type.show @comp <> " (" <> show n <> ")"
        let compSize = Layer.byteSize @layers
        pool       <- State.get @(MemPool (Component comp ()))
        tgtPtr'    <- MemPool.allocN pool n
        let tgtPtr      = wrap $ coerce tgtPtr'
            bytesToCopy = compSize * n
        Memory.copyBytes tgtPtr srcPtr bytesToCopy
        let nextSrcPtr = srcPtr `Memory.plus` bytesToCopy
        (map, regions) <- decodeStaticComponents__ @comps ns nextSrcPtr
        pure $ ((tgtPtr `Memory.minus` srcPtr) : map, tgtPtr : regions)
    {-# INLINE decodeStaticComponents__ #-}







--------------------------------
-- === CopyInitialization2 === --
--------------------------------

-- === Definition === --

data CopyInitialization2
type instance Fold.Result     CopyInitialization2 = ()
type instance Fold.LayerScope CopyInitialization2 = 'Fold.All

instance Monad m => Fold.ComponentBuilder CopyInitialization2 m comp

instance (Monad m, CopyInitializerP1_2 m (Layer.Cons layer))
      => Fold.LayerBuilder CopyInitialization2 m layer where
        layerBuild = \layer m -> m <* copyInitializeP1_2 layer


-- === CopyInitializerP1_2 === --

class Applicative m => CopyInitializerP1_2 m a where
    copyInitializeP1_2 :: ∀ t1. a t1 -> m (a t1)
    copyInitializeP1_2 = pure
    {-# INLINE copyInitializeP1_2 #-}

instance (Data.CopyInitializer1 m (ComponentSet comp), Applicative m)
      => CopyInitializerP1_2 m (ComponentSet comp) where
    copyInitializeP1_2 = \a -> a <$ Data.copyInitialize1 a

instance Applicative m => CopyInitializerP1_2 m (Component comp)

instance {-# OVERLAPPABLE #-}
    ( Fold.Builder1 (Fold.Struct CopyInitialization2) m a
    , Applicative m
    ) => CopyInitializerP1_2 m a where
    copyInitializeP1_2 = \a -> a <$ (Fold.build1 @(Fold.Struct CopyInitialization2) a (pure ()))


-- === CopyInitialization2 Folds === --

instance (Data.CopyInitializer1 m (ComponentSet comp), Monad m)
      => Fold.Builder1 CopyInitialization2 m (ComponentSet comp) where
    build1 = \a x -> x <* Data.copyInitialize1 a
    {-# INLINE build1 #-}

instance (Data.CopyInitializer1 m (ComponentVector comp), Monad m)
      => Fold.Builder1 CopyInitialization2 m (ComponentVector comp) where
    build1 = \a x -> x <* Data.copyInitialize1 a
    {-# INLINE build1 #-}

instance (Data.CopyInitializer m (SmallVectorA t alloc comp a), Monad m)
      => Fold.Builder CopyInitialization2 m (SmallVectorA t alloc comp a) where
    build = \a x -> x <* Data.copyInitialize a
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} (Monad m, Fold.Builder CopyInitialization2 m a)
      => Fold.Builder CopyInitialization2 m (Maybe a) where
    build = \m x -> maybe x (\t -> Fold.build @CopyInitialization2 t x) m
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-}
    ( Monad m, Fold.Builder CopyInitialization2 m a
    , Fold.Builder CopyInitialization2 m b
    ) => Fold.Builder CopyInitialization2 m (a, b) where
    build = \(a, b) x -> Fold.build @CopyInitialization2 a
                       $ Fold.build @CopyInitialization2 b x
    {-# INLINE build #-}


instance Monad m => Fold.Builder1 CopyInitialization2 m (Component comp)




-----------------------------
-- === CopyInitializer2 === --
-----------------------------

-- === Definition === --

type ComponentStaticInitializer2 m =
    ( ComponentStaticInitializer2__ (Graph.ComponentsM m) m
    , Graph.KnownComponentNumber (Graph.Discover m)
    , MonadIO m
    )

copyInitializeComponents2 :: ∀ m. ComponentStaticInitializer2 m
    => [Int] -> [Memory.SomeUnmanagedPtr] -> m ()
copyInitializeComponents2 ccount regions = do
    copyInitializeComponents2__ @(Graph.ComponentsM m) ccount regions
{-# INLINE copyInitializeComponents2 #-}



-- === ComponentInitializerFold === --

type ComponentInitializerFold2 m comp
   = Fold.Builder1 (Fold.Scoped CopyInitialization2) m (Component comp)

foldInitializeComponent2 :: ComponentInitializerFold2 m comp => Component comp layout -> m ()
foldInitializeComponent2 = \comp -> Fold.build1 @(Fold.Scoped CopyInitialization2) comp (pure ())

class ComponentStaticInitializer2__ (comps :: [Type]) m where
    copyInitializeComponents2__ :: [Int] -> [Memory.SomeUnmanagedPtr] -> m ()

instance Applicative m
      => ComponentStaticInitializer2__ '[] m where
    copyInitializeComponents2__ = \_ _ -> pure ()
    {-# INLINE copyInitializeComponents2__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentInitializerFold2 m comp
    , ComponentStaticInitializer2__ comps m
    , MonadIO m
    -- debug:
    , Type.Show comp
    )
      => ComponentStaticInitializer2__ (comp ': comps) m where
    copyInitializeComponents2__ = \(compCount : compCounts) (region : regions) -> do
        let compSize   = Layer.byteSize @layers
        putStrLn $ "copyInitializeComponents2 for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ ">> " <> show ix <> " (" <> show region <> ")"
            let ptr  = region `Memory.plus` (ix * compSize)
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldInitializeComponent2 comp
        copyInitializeComponents2__ @comps compCounts regions






---------------------------------
-- === Pointer redirection === --
---------------------------------


type ComponentStaticRedirection2 comps m
    = ( ComponentStaticRedirection2__ comps (State.StateT DecodeOffsetMap m)
      , Monad m
      )
redirectComponents2 :: ∀ comps m. ComponentStaticRedirection2 comps m
    => DecodeOffsetMap -> [Int] -> [Memory.SomeUnmanagedPtr] -> m ()
redirectComponents2 = \m ccount region -> flip (State.evalT @DecodeOffsetMap) m
    $ redirectComponents2__ @comps ccount region


class ComponentStaticRedirection2__ (comps :: [Type]) m where
    redirectComponents2__ :: [Int] -> [Memory.SomeUnmanagedPtr] -> m ()

instance Applicative m
      => ComponentStaticRedirection2__ '[] m where
    redirectComponents2__ = \_ _ -> pure ()
    {-# INLINE redirectComponents2__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentRedirectFold2 m comp
    , ComponentStaticRedirection2__ comps m
    , MonadIO m
    -- debug:
    , Type.Show comp
    )
      => ComponentStaticRedirection2__ (comp ': comps) m where
    redirectComponents2__ = \(compCount : compCounts) (region : regions) -> do
        let compSize   = Layer.byteSize @layers
        putStrLn $ "redirectComponents2 for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ ">> " <> show ix <> " (" <> show region <> ")"
            let ptr  = region `Memory.plus` (ix * compSize)
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldRedirectComponent2 comp
        redirectComponents2__ @comps compCounts regions

data ComponentRedirection2
type instance Fold.Result     ComponentRedirection2 = ()
type instance Fold.LayerScope ComponentRedirection2 = 'Fold.All

instance Monad m => Fold.ComponentMap ComponentRedirection2 m comp

instance (MonadIO m, PointerRedirection1_2 m (Layer.Cons layer), State.Getter DecodeOffsetMap m)
      => Fold.LayerMap ComponentRedirection2 m layer where
        mapLayer = \a _ -> do
            m <- State.get @DecodeOffsetMap
            -- let f ptr = do
            --         putStrLn $ "LOOKUP " <> show ptr
            --         pure . unsafeFromJust . flip Map.lookup m $ ptr
            (,()) <$> redirectPointers1_2 m a

            -- Fold.build1 @ComponentRedirection2



type ComponentRedirectFold2 m comp
   = Fold.Builder1 (Fold.ScopedMap ComponentRedirection2) m (Component comp)

foldRedirectComponent2 :: ComponentRedirectFold2 m comp => Component comp layout -> m ()
foldRedirectComponent2 = \comp -> Fold.build1 @(Fold.ScopedMap ComponentRedirection2) comp (pure ())

class PointerRedirection1_2 m a where
    redirectPointers1_2 :: DecodeOffsetMap
                      -> a t1 -> m (a t1)
    default redirectPointers1_2 :: Applicative m
                                => DecodeOffsetMap
                                -> a t1 -> m (a t1)
    redirectPointers1_2 = \_ -> pure

class PointerRedirection_2 m a where
    redirectPointers_2 :: DecodeOffsetMap
                     -> a -> m a

    default redirectPointers_2 :: Applicative m => DecodeOffsetMap -> a -> m a
    redirectPointers_2 = \_ -> pure

instance
    -- ( comps ~ Graph.ComponentsM m
    -- , idx   ~ List.Index' tag comps
    -- , Type.KnownInt idx
    ( MonadIO m, PointerRedirection1_2 m (Component tag)
    ) => PointerRedirection1_2 m (ComponentSetA alloc tag) where
    redirectPointers1_2 = \f a -> do
        -- let idx = Type.val' @idx :: Int
        putStrLn ">> redirection ComponentSetA"
        out <- a <$ Mutable.mapM a (redirectPointers1_2 f)
        putStrLn ">> redirection ComponentSetA"
        pure out
    {-# INLINE redirectPointers1_2 #-}

instance
    ( comps ~ Graph.ComponentsM m
    , idx   ~ List.ElemIndex' comp comps
    , Type.KnownInt idx
    , MonadIO m
    ) => PointerRedirection1_2 m (Component comp) where
    redirectPointers1_2 = \f a ->
        let idx  = Type.val' @idx :: Int
            off  = f !! idx
            ptr  = wrap $ Component.unsafeToPtr a :: Memory.UnmanagedPtr ()
            ptr' = ptr `Memory.plus` off
        in do
            putStrLn $ "shift: " <> show off
            pure $ Component.unsafeFromPtr (unwrap ptr')


instance (MonadIO m, PointerRedirection1_2 m (Component tag))
      => PointerRedirection_2 m (ComponentVectorA alloc tag layout) where
    redirectPointers_2 = \f a -> do
        putStrLn ">> redirection ComponentVectorA"
        out <- a <$ Mutable.mapM a (redirectPointers1_2 f)
        putStrLn "<< redirection ComponentVectorA"
        pure out
    {-# INLINE redirectPointers_2 #-}

instance (Applicative m, PointerRedirection1_2 m (Component comp))
      => PointerRedirection_2 m (Component comp layout) where
    redirectPointers_2 = redirectPointers1_2

instance (Applicative m, PointerRedirection1_2 m (Component comp))
      => PointerRedirection1_2 m (MaybeComponent comp) where
    redirectPointers1_2 = MaybeComponent.mapM . redirectPointers1_2
    {-# INLINE redirectPointers1_2 #-}

instance Applicative m => PointerRedirection1_2 m (Layer.Simple a)

instance Applicative m => PointerRedirection_2 m Int
instance Applicative m => PointerRedirection_2 m Word8
instance Applicative m => PointerRedirection_2 m Word16
instance Applicative m => PointerRedirection_2 m Word32
instance Applicative m => PointerRedirection_2 m Word64
instance Applicative m => PointerRedirection_2 m Bool

instance
    ( ctx ~ PointerRedirection_2 m
    , MonadIO m
    , PointerRedirection1_2 m (Component Edge.Edges)
    ) => PointerRedirection1_2 m IR.UniTerm where
    redirectPointers1_2 = \f
        -> GTraversable.gmapM @(GTraversable.GTraversable ctx)
         $ GTraversable.gmapM @ctx (redirectPointers_2 f)

instance Applicative m => PointerRedirection_2 m (SmallVectorA t alloc n IR.Name)
instance Applicative m => PointerRedirection_2 m (SmallVectorA t alloc n IR.Qualified)
instance Applicative m => PointerRedirection_2 m (SmallVectorA t alloc n Char)
instance Applicative m => PointerRedirection_2 m (SmallVectorA t alloc n Word8)
instance Applicative m => PointerRedirection_2 m IR.Name
instance Applicative m => PointerRedirection_2 m IR.Qualified
instance Applicative m => PointerRedirection_2 m IR.ForeignImportType
instance Applicative m => PointerRedirection_2 m IR.ImportSourceData
instance Applicative m => PointerRedirection_2 m IR.ImportTargetData
instance Applicative m => PointerRedirection_2 m InvalidIR.Symbol


-- newtype Header = Header
--     { _size :: Size
--     } deriving (Show)
-- makeLenses      ''Header
-- Storable.derive ''Header

-- data Buffer = Buffer
--     { _header :: Header
--     , _block  :: SomeForeignPtr
--     } deriving (Show)
-- makeLenses ''Buffer

-- data BufferView = BufferView
--     { _static  :: SomePtr
--     , _dynData :: SomePtr
--     , _dynPtrs :: SomePtr
--     } deriving (Show)
-- makeLenses ''BufferView


-- alloc :: MonadIO m => Size -> m Buffer
-- alloc = \size -> do
--     let headerSize = Storable.sizeOf' @Header
--         bodySize   = Size.total size
--         totalSize  = headerSize + bodySize
--     ptr <- liftIO $ ForeignPtr.mallocForeignPtrBytes totalSize
--     let bodyPtr = ForeignPtr.plusForeignPtr ptr headerSize
--     pure $ Buffer (Header size) bodyPtr
-- {-# INLINE alloc #-}


-- === API === --

-- instance Convert.To ByteString Buffer where
--     to = \(Data ptr size) -> ByteString.PS (coerce ptr) 0 (Size.total size)
--     {-# INLINE to #-}

-- data Dynamic = Dynamic
--     { _noPointersMem :: SomePtr
--     , _pointersMem   :: SomePtr
--     } deriving (Show)
-- makeLenses ''Dynamic

-- type DynamicMemVariant = Lens' Dynamic SomePtr

-- unsafeMakeRaw :: Data -> Raw
-- unsafeMakeRaw = \(Data staticMem dynamicMem ptrsMem) ->
--     let staticPtr  = unsafeForeignPtrToPtr staticMem
--         dynamicPtr = unsafeForeignPtrToPtr dynamicMem
--         ptrsPtr    = unsafeForeignPtrToPtr ptrsMem
--     in Raw staticPtr dynamicPtr ptrsPtr
-- {-# INLINE unsafeMakeRaw #-}

-- touch :: MonadIO m => Data -> m ()
-- touch = \(Data staticMem dynamicMem ptrsMem) -> liftIO $! do
--     touchForeignPtr staticMem
--     touchForeignPtr dynamicMem
--     touchForeignPtr ptrsMem
-- {-# INLINE touch #-}

-- withRaw :: MonadIO m
--         => Data -> (Raw -> m Raw)
--         -> m Data
-- withRaw = \memReg f -> do
--     let rawMemReg = unsafeMakeRaw memReg
--     _ <- f rawMemReg
--     touch memReg
--     pure $! memReg
-- {-# INLINE withRaw #-}

-- viewDynamic :: Raw -> Dynamic
-- viewDynamic = \(Raw _ dm dpm) -> Dynamic dm dpm
-- {-# INLINE viewDynamic #-}

-- constructRaw :: Dynamic -> SomePtr -> Raw
-- constructRaw = \(Dynamic dm dpm) ptr -> Raw ptr dm dpm
-- {-# INLINE constructRaw #-}


-- data Data = Data
--     { _staticMem  :: SomeForeignPtr
--     , _dynamicMem     :: SomeForeignPtr
--     , _dynamicPtrsMem :: SomeForeignPtr
--     } deriving (Show)
-- makeLenses ''Data
