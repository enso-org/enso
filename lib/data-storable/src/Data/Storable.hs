{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Storable where

import Prologue hiding (Lens, lens, read)

import qualified Data.Convert2.Class    as Convert
import qualified Foreign.ForeignPtr     as ForeignPtr
import qualified Foreign.Marshal.Alloc  as Mem
import qualified Foreign.Marshal.Utils  as Mem
import qualified Foreign.Ptr            as Ptr
import qualified Foreign.Storable.Class as Storable
import qualified Memory                 as Memory
import qualified Type.Data.List         as List
import qualified Type.Known             as Type

import Foreign.ForeignPtr        (ForeignPtr, plusForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr               (plusPtr)
import Foreign.Storable.Class    (Storable)
import Type.Data.List            (type (<>))




data Fieldx




----------------------
-- === FieldRef === --
----------------------

-- === Definition === --

newtype FieldRef   t a    = FieldRef (Memory.Ptr t a)
type    FieldRefOf a      = FieldRef (Memory.Management a)
type    UnmanagedFieldRef = FieldRef 'Memory.Unmanaged
type    ManagedFieldRef   = FieldRef 'Memory.Managed
makeLenses ''FieldRef



-------------------
-- === Field === --
-------------------

-- === Definition === --

data Field (path :: [Symbol]) = Field

data FieldSig = FieldSig Symbol Type
type name -:: tp = 'FieldSig name tp

type family FieldSigType field where FieldSigType ('FieldSig _ t) = t
type family FieldSigName field where FieldSigName ('FieldSig n _) = n

type family MapFieldSigType fields where
    MapFieldSigType (f ': fs) = FieldSigType f ': MapFieldSigType fs
    MapFieldSigType '[]       = '[]

type family MapFieldSigName fields where
    MapFieldSigName (f ': fs) = FieldSigName f ': MapFieldSigName fs
    MapFieldSigName '[]       = '[]

instance Storable.KnownConstantSize a
      => Storable.KnownConstantSize ('FieldSig n a) where
    constantSize = Storable.constantSize @a
    {-# INLINE constantSize #-}


-- === FieldType === --

type family FieldType field a where
    FieldType field Imp     = Imp
    FieldType (Field '[]) a = a
    FieldType field       a = LookupFieldType field (Fields a)

type family LookupFieldType path fields where
    LookupFieldType (Field (n ': ns)) (('FieldSig n v) ': _)
        = FieldType (Field ns) v
    LookupFieldType field (_ ': fs) = LookupFieldType field fs



------------------
-- === Lens === --
------------------

-- === Definition === --

type Lens t = (∀ a. Field a -> Field (ToPath t <> a))

type family ToPath (s :: k) :: [Symbol] where
    ToPath (s :: Symbol)   = '[s]
    ToPath (s :: [Symbol]) = s


-- === Utils === --

type AppliedLens a = Field '[] -> a

lens :: ∀ s. Lens s
lens = \_ -> Field
{-# INLINE lens #-}

autoLens :: Field a -> Field b
autoLens = \_ -> Field
{-# INLINE autoLens #-}



--------------------
-- === Struct === --
--------------------

-- === Definition === --

type    Struct__ t (fields :: [FieldSig]) = Memory.SomePtr t
newtype Struct   t (fields :: [FieldSig]) = Struct (Struct__ t fields)


-- === Aliases === --

type ManagedStruct   = Struct 'Memory.Managed
type UnmanagedStruct = Struct 'Memory.Unmanaged


-- === Generalization === --

class IsStruct a where
    type Fields a :: [FieldSig]
    struct :: Iso' a (Struct (Memory.Management a) (Fields a))

    type Fields a = Fields (Unwrapped a)
    default struct
        :: (Wrapped a, Unwrapped a ~ Struct (Memory.Management a) (Fields a))
        => Iso' a (Struct (Memory.Management a) (Fields a))
    struct = wrapped'
    {-# INLINE struct #-}

instance IsStruct (Struct t fields) where
    type Fields (Struct t fields) = fields
    struct = id
    {-# INLINE struct #-}


-- === Instances === --

makeLenses ''Struct

type instance Memory.Management (Struct t _) = t

instance Storable.KnownConstantSize (MapFieldSigType fields)
      => Storable.KnownConstantSize (Struct t fields) where
    constantSize = Storable.constantSize @(MapFieldSigType fields)
    {-# INLINE constantSize #-}

deriving instance Eq     (Struct__ t fields) => Eq     (Struct t fields)
deriving instance NFData (Struct__ t fields) => NFData (Struct t fields)
deriving instance Ord    (Struct__ t fields) => Ord    (Struct t fields)
deriving instance Show   (Struct__ t fields) => Show   (Struct t fields)



-----------------------------
-- === Reader / Writer === --
-----------------------------

-- === Definition === --

type Editor field a = (Reader field a, Writer field a)

class Reader (lens :: Type) (a :: Type) where
    readFieldIO :: a -> IO (FieldType lens a)

class Writer (field :: Type) (a :: Type) where
    writeFieldIO :: a -> FieldType field a -> IO ()


-- === Types === --

type FieldRefPeek t a = (Memory.PtrType t, Storable.Peek Fieldx IO a)
type FieldRefPoke t a = (Memory.PtrType t, Storable.Poke Fieldx IO a)


-- === API === --

read :: ∀ field a m. (Reader field a, MonadIO m)
     => AppliedLens field -> a -> m (FieldType field a)
read = const $ readField @field
{-# INLINE read #-}

write :: ∀ field a m. (Writer field a, MonadIO m)
    => AppliedLens field -> a -> FieldType field a -> m ()
write = const $ writeField @field
{-# INLINE write #-}

readField :: ∀ lens a m. (Reader lens a, MonadIO m)
    => a -> m (FieldType lens a)
readField = liftIO . readFieldIO @lens
{-# INLINE readField #-}

writeField :: ∀ field a m. (Writer field a, MonadIO m)
    => a -> FieldType field a -> m ()
writeField = liftIO .: writeFieldIO @field
{-# INLINE writeField #-}

peekRef :: (FieldRefPeek t a, MonadIO m) => FieldRef t a -> m a
peekRef = \a -> liftIO $ Memory.withUnmanagedPtr (unwrap a)
                       $ Storable.peek @Fieldx
{-# INLINE peekRef #-}

pokeRef :: (FieldRefPoke t a, MonadIO m) => FieldRef t a -> a -> m ()
pokeRef = \a v -> liftIO $ Memory.withUnmanagedPtr (unwrap a)
                         $ flip (Storable.poke @Fieldx) v
{-# INLINE pokeRef #-}


-- === Instances === --

instance (Has field a, FieldRefPeek (Memory.Management a) (FieldType field a))
      => Reader field a where
    readFieldIO = peekRef . fieldRef @field
    {-# INLINE readFieldIO #-}

instance (Has field a, FieldRefPoke (Memory.Management a) (FieldType field a))
      => Writer field a where
    writeFieldIO = pokeRef . fieldRef @field
    {-# INLINE writeFieldIO #-}


-- === Early resolution block === --

type ImpField = Field '[ImpSymbol]
instance {-# OVERLAPPABLE #-} Reader ImpField a   where readFieldIO = impossible
instance {-# OVERLAPPABLE #-} Reader field    Imp where readFieldIO = impossible



-----------------
-- === Has === --
-----------------

-- === Definition === --

type  HasCtx a = (IsStruct a, Memory.PtrType (Memory.Management a))
class HasCtx a => Has (field :: Type) (a :: Type) where
    byteOffset :: Int


-- === Utils === --

fieldRef :: ∀ field a. Has field a => a -> FieldRefOf a (FieldType field a)
fieldRef = \a ->
    let ptr = Memory.coercePtr . unwrap $ a ^. struct
        off = byteOffset @field @a
    in FieldRef $! ptr `Memory.plus` off
{-# INLINE fieldRef #-}

ref :: ∀ field a. Has field a
    => AppliedLens field -> a -> FieldRefOf a (FieldType field a)
ref = const $ fieldRef @field
{-# INLINE ref #-}


-- === Instances ==== --

class HasField__
    (man    :: Memory.ManagementType)
    (label  :: Symbol)
    (labels :: [Symbol])
    (fs     :: [FieldSig])
    (idx    :: Maybe Nat) where
    byteOffset__ :: Int

instance HasCtx a => Has (Field '[]) a where
    byteOffset = 0
    {-# INLINE byteOffset #-}

instance
    ( fields ~ Fields a
    , idx    ~ List.ElemIndex label (MapFieldSigName fields)
    , man    ~ Memory.Management a
    , HasCtx a
    , HasField__ man label labels fields idx
    ) => Has (Field (label ': labels)) a where
    byteOffset = byteOffset__ @man @label @labels @fields @idx
    {-# INLINE byteOffset #-}

instance {-# OVERLAPPABLE #-}
    ( types    ~ MapFieldSigType fields
    , fields'  ~ List.Take   idx types
    , tgtType  ~ List.Index' idx types
    , tgtField ~ Field labels
    , Storable.KnownConstantSize fields'
    , Has tgtField tgtType
    ) => HasField__ t label labels fields ('Just idx) where
    byteOffset__ = Storable.constantSize @fields'
                 + byteOffset @tgtField @tgtType
    {-# INLINE byteOffset__ #-}

instance
    ( types    ~ MapFieldSigType fields
    , fields'  ~ List.Take   idx types
    , tgtType  ~ List.Index' idx types
    , Storable.KnownConstantSize fields'
    ) => HasField__ t label '[] fields ('Just idx) where
    byteOffset__ = Storable.constantSize @fields'
    {-# INLINE byteOffset__ #-}



--------------------------
-- === Construction === --
--------------------------

-- === Definition === --

type ConstructorSig    a        = ConsSig__ a (MapFieldSigType (Fields a))
type StructConstructor a fields = Cons__  a (MapFieldSigType fields)
type Allocator  t               = IO (Memory.SomePtr t)
type Serializer t               = Memory.SomePtr t -> (IO (), Memory.SomePtr t)
type Constructor       a        = ( StructConstructor a (Fields a)
                                  , ConsTgt__ (ConstructorSig a) ~ a
                                  )

placementWith :: ∀ a. Constructor a
    => Allocator (Memory.Management a) -> ConstructorSig a
placementWith = \alloc -> cons__ @a @(MapFieldSigType (Fields a))
                          alloc (\ptr -> (pure (), ptr))
{-# INLINE placementWith #-}

placementNew :: ∀ a. Constructor a
    => Memory.Ptr (Memory.Management a) a -> ConstructorSig a
placementNew = placementWith . pure . Memory.coercePtr
{-# INLINE placementNew #-}

construct :: ∀ a.
    ( Constructor a
    , Memory.PtrType (Memory.Management a)
    , Storable.KnownConstantSize (Fields a)
    ) => ConstructorSig a
construct = placementWith @a (Memory.mallocBytes size) where
    size = Storable.constantSize @(Fields a)
{-# INLINE construct #-}

free :: ∀ a m. (IsStruct a, MonadIO m, Memory.AssertUnmanaged a) => a -> m ()
free = liftIO . Mem.free . unwrap . unwrap . view struct
{-# INLINE free #-}

unsafeCastFromPtr :: ∀ a p. IsStruct a
    => Memory.Ptr (Memory.Management a) a -> a
unsafeCastFromPtr = view (from struct)
    . Struct @(Memory.Management a) @(Fields a) . Memory.coercePtr
{-# INLINE unsafeCastFromPtr #-}


-- === Internal === --

type family ConsSig__ a types where
    ConsSig__ a (t ': ts) = t -> (ConsSig__ a ts)
    ConsSig__ a '[]       = IO a

type family ConsTgt__ f where
    ConsTgt__ (_ -> a) = ConsTgt__ a
    ConsTgt__ (IO a)   = a

class Cons__ a types where
    cons__ :: Allocator  (Memory.Management a)
           -> Serializer (Memory.Management a)
           -> ConsSig__ a types

instance {-# OVERLAPPABLE #-}
    ( Cons__ a fs
    , Storable.KnownConstantSize f
    , Storable.Poke Fieldx IO f
    , Memory.PtrType (Memory.Management a)
    ) => Cons__ a (f ': fs) where
    cons__ = \alloc f a -> cons__ @a @fs alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Memory.withUnmanagedRawPtr ptr'
                               (flip (Storable.poke @Fieldx) a . coerce)
            ptr''       = ptr' `Memory.plus` Storable.constantSize @f
        in  (f', ptr'')
    {-# INLINE cons__ #-}

instance
    ( Cons__ a '[]
    , Storable.Poke Fieldx IO f
    , Memory.PtrType (Memory.Management a)
    ) => Cons__ a '[f] where
    cons__ = \alloc f a -> cons__ @a @('[]) alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Memory.withUnmanagedRawPtr ptr'
                               (flip (Storable.poke @Fieldx) a . coerce)
        in  (f', ptr')
    {-# INLINE cons__ #-}

instance IsStruct a
      => Cons__ a '[] where
    cons__ = \alloc f -> do
        ptr <- alloc
        let (!m, !_) = f ptr
        (Struct ptr ^. from struct) <$ m
    {-# INLINE cons__ #-}









type Foo = Lens "foo"
type Bar = Lens "bar"
type Baz = Lens "baz"

newtype X = X (Struct 'Memory.Unmanaged '["foo" -:: Int, "bar" -:: Int, "baz" -:: Int])
makeLenses ''X

type instance Memory.Management X = 'Memory.Unmanaged

instance IsStruct X

foo :: Lens "foo"
foo = lens @"foo"

bar :: Lens "bar"
bar = lens @"bar"

baz :: Lens "baz"
baz = autoLens

xxx :: Lens '["foo", "bar"]
xxx = foo . bar

-- -- -- Struct.Has (Lens "foo") a

test :: Int -> Int -> Int -> IO X
test = construct @X

main :: IO ()
main = do
    a <- test 1 2 3
    print =<< read foo a
    print =<< read bar a
    print =<< read baz a
    write foo a 10
    print =<< read foo a

    print "end"

