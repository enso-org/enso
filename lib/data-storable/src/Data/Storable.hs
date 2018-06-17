{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Storable where

import Prologue hiding (product)

import qualified Foreign.Marshal.Alloc  as Mem
import qualified Foreign.Marshal.Utils  as Mem
import qualified Foreign.Storable.Class as Storable
import qualified Type.Data.List         as List
import qualified Type.Known             as Type

import Foreign.Ptr            (Ptr, plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable.Class (Storable)
import Type.Data.Semigroup    (type (<>))



-- ----------------------
-- -- === ByteSize === --
-- ----------------------

-- -- === Definition === --

-- type family ByteSize t (a :: k) :: Nat


-- -- === API === --

-- type KnownByteSize t a = Type.KnownInt (ByteSize t a)
-- size :: ∀ t a. KnownByteSize t a => Int
-- size = Type.val' @(ByteSize t a)
-- {-# INLINE size #-}

-- type family SumByteSizes t (ls :: [k]) where
--     SumByteSizes t '[]       = 0
--     SumByteSizes t (a ': as) = ByteSize t a + ByteSize t as


-- -- === Instances === --

-- type instance ByteSize t (a :: [k]) = SumByteSizes t a
-- type instance ByteSize _ Int     = 8
-- type instance ByteSize _ (Ptr _) = 8



-------------------
-- === Field === --
-------------------

-- === Field reference === --

data Field
data FieldRef (name :: Symbol) = FieldRef deriving (Show)

field :: ∀ name. FieldRef name
field = FieldRef
{-# INLINE field #-}


-- === API === --

type KnownFieldSize a = Storable.KnownStaticSize Field a
fieldSize :: ∀ a. KnownFieldSize a => Int
fieldSize = Storable.staticSize @Field @a
{-# INLINE fieldSize #-}


-- === Field signature === --

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

instance Storable.KnownStaticSize t a
      => Storable.KnownStaticSize t ('FieldSig n a) where
    staticSize = Storable.staticSize @t @a


-- === FieldType === --

type FieldType name a = LookupFieldType name (Fields a)
type LookupFieldType name fields = LookupFieldType__ name fields

type family LookupFieldType__ name fields where
    LookupFieldType__ n (('FieldSig n v) ': _) = v
    LookupFieldType__ n (_ ': fs)              = LookupFieldType__ n fs



-----------------------
-- === Struct === --
-----------------------

-- === Definition === --

newtype Struct (fields :: [FieldSig]) = Struct SomePtr deriving (Show, NFData)

class IsStruct a where
    type Fields a :: [FieldSig]
    struct :: Iso' a (Struct (Fields a))

    type Fields a = Fields (Unwrapped a)
    default struct :: (Wrapped a, Unwrapped a ~ Struct (Fields a))
                   => Iso' a (Struct (Fields a))
    struct = wrapped' ; {-# INLINE struct #-}

instance IsStruct (Struct fields) where
    type Fields (Struct fields) = fields
    struct = id ; {-# INLINE struct #-}


-- === Helpers === --

-- type StructByteSize      a = ByteSize Field a
-- type KnownStructByteSize a = Type.KnownInt (StructByteSize a)

-- structByteSize :: ∀ a. KnownStructByteSize a => Int
-- structByteSize = Type.val' @(StructByteSize a)
-- {-# INLINE structByteSize #-}


-- === Instances === --
makeLenses ''Struct



-----------------------------------
-- === Field Reader / Writer === --
-----------------------------------

-- === API === --

fieldPtr :: ∀ name a. HasField name a
         => FieldRef name -> a -> Ptr (FieldType name a)
fieldPtr = \_ -> fieldPtrByName @name
{-# INLINE fieldPtr #-}

readField :: ∀ name a m. (FieldReader name a, MonadIO m)
          => FieldRef name -> a -> m (FieldType name a)
readField = \_ -> liftIO . readFieldByNameIO @name
{-# INLINE readField #-}

writeField :: ∀ name a m. (FieldWriter name a, MonadIO m)
    => FieldRef name -> a -> FieldType name a -> m ()
writeField = \_ -> liftIO .: writeFieldByNameIO @name
{-# INLINE writeField #-}

modifyField :: ∀ name t a m. (FieldEditor name a, MonadIO m)
    => FieldRef name -> (FieldType name a -> m (t, FieldType name a)) -> a -> m t
modifyField = \field f a -> do
    v <- readField field a
    (!t, !v') <- f v
    writeField field a v'
    pure t
{-# INLINE modifyField #-}

modifyField_ :: ∀ name t a m. (FieldEditor name a, MonadIO m)
    => FieldRef name -> (FieldType name a -> m (FieldType name a)) -> a -> m ()
modifyField_ = \field f a -> do
    v  <- readField field a
    v' <- f v
    writeField field a v'
{-# INLINE modifyField_ #-}


-- === Class === --

type FieldEditor name a = (FieldReader name a, FieldWriter name a)

class HasField (name :: Symbol) a where
    fieldPtrByName :: a -> Ptr (FieldType name a)

class FieldReader (name :: Symbol) a where
    readFieldByNameIO :: a -> IO (FieldType name a)

class FieldWriter (name :: Symbol) a where
    writeFieldByNameIO :: a -> FieldType name a -> IO ()


-- === Internal === --

class HasField__ (name :: Symbol) (fs :: [FieldSig]) (idx :: Maybe Nat) where
    fieldPtr__ :: Struct fs -> Ptr (LookupFieldType name fs)

instance
    ( fields' ~ List.Take idx (MapFieldSigType fields)
    , Storable.KnownStaticSize Field fields'
    ) => HasField__ name fields ('Just idx) where
    fieldPtr__ = \(Struct !ptr) ->
        let off = Storable.staticSize @Field @fields'
        in  ptr `plusPtr` off
    {-# INLINE fieldPtr__ #-}

instance (HasField name a, Storable.Peek Field IO (FieldType name a))
      => FieldReader name a where
    readFieldByNameIO = \a -> Storable.peek @Field (fieldPtrByName @name a)
    {-# INLINE readFieldByNameIO #-}

instance (HasField name a, Storable.Poke Field IO (FieldType name a))
      => FieldWriter name a where
    writeFieldByNameIO = \a val -> Storable.poke @Field (fieldPtrByName @name a) val
    {-# INLINE writeFieldByNameIO #-}

instance
    ( fields ~ Fields a
    , idx    ~ List.ElemIndex name (MapFieldSigName fields)
    , IsStruct a
    , HasField__ name fields idx
    ) => HasField name a where
    fieldPtrByName = fieldPtr__ @name @fields @idx . view struct
    {-# INLINE fieldPtrByName #-}




--------------------------
-- === Construction === --
--------------------------

-- === Definition === --

type Constructor       a        = StructConstructor a (Fields a)
type ConstructorSig    a        = ConsSig__ a (MapFieldSigType (Fields a))
type StructConstructor a fields = Cons__  a (MapFieldSigType fields)
type Allocator                  = Int -> IO SomePtr
type Serializer                 = SomePtr -> (IO (), SomePtr)

constructWith :: ∀ a. Constructor a => Allocator -> ConstructorSig a
constructWith = \alloc -> cons__ @a @(MapFieldSigType (Fields a))
                          alloc (\ptr -> (pure (), ptr))
{-# INLINE constructWith #-}

construct :: ∀ a. Constructor a => ConstructorSig a
construct = constructWith @a Mem.mallocBytes
{-# INLINE construct #-}

free :: ∀ a m. (IsStruct a, MonadIO m) => a -> m ()
free = liftIO . Mem.free . unwrap . view struct
{-# INLINE free #-}

unsafeCastFromPtr :: ∀ a t. IsStruct a => Ptr t -> a
unsafeCastFromPtr = \ptr -> view (from struct) $ Struct @(Fields a) (coerce ptr)
{-# INLINE unsafeCastFromPtr #-}


-- === Internal === --

type family ConsSig__ a types where
    ConsSig__ a (t ': ts) = t -> (ConsSig__ a ts)
    ConsSig__ a '[]       = IO a

class Cons__ a types where
    cons__ :: Allocator -> Serializer -> ConsSig__ a types

instance
    ( Cons__ fields fs
    , KnownFieldSize f
    , Storable.Poke Field IO f
    ) => Cons__ fields (f ': fs) where
    cons__ = \alloc f a -> cons__ @fields @fs alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Storable.poke @Field (coerce ptr') a
            ptr''       = ptr' `plusPtr` fieldSize @f
        in  (f', ptr'')
    {-# INLINE cons__ #-}

instance (IsStruct a, Storable.KnownStaticSize Field (Fields a))
      => Cons__ a '[] where
    cons__ = \alloc f -> do
        ptr <- alloc $! Storable.staticSize @Field @(Fields a)
        let (!m, !_) = f ptr
        (Struct ptr ^. from struct) <$ m
    {-# INLINE cons__ #-}









    -- type Fields a = Fields (U)


newtype X = X (Struct '["foo" -:: Int, "bar" -:: Int, "baz" -:: Int])
makeLenses ''X

instance IsStruct X

foo :: FieldRef "foo"
bar :: FieldRef "bar"
baz :: FieldRef "baz"
foo = field
bar = field
baz = field

test :: Int -> Int -> Int -> IO X
test = construct @X

main :: IO ()
main = do
    a <- test 1 2 3
    print =<< readField foo a
    print =<< readField bar a
    print =<< readField baz a
    writeField foo a 10
    print =<< readField foo a

    print "end"


-- construct :: ∀ fields. Constructor fields
--           => MemAllocator -> ConstructorSig fields
-- construct malloc = allocProduct @fields malloc (\ptr -> (pure (), ptr))
-- {-# INLINE construct #-}

-- var :: Int -> IO Varx
-- var = product @VarLayout Mem.mallocBytes
-- {-# NOINLINE var #-}

-- match :: SomePtr -> Vector SomePtr -> IO Matchx
-- match = product @MatchLayout Mem.mallocBytes
-- {-# NOINLINE match #-}

-- main :: IO ()
-- main = do
--     v <- var 7
--     print =<< viewField @"name" v
--     -- print $ Type.val' @(ByteSize Varx)
--     print "end"
-- -- instance Storable (Struct layout)

-- -- data Model



-- -- type Node = Struct '[] '[Model]

