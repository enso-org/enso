module OCI.Data.Name where

import Prologue

import qualified Data.IntMap.Strict as IntMap
import qualified FastString         as FastString
import qualified Prelude            as Prelude

import Binary           (Binary)
import Data.IntMap      (IntMap)
import Data.IORef       (IORef, atomicModifyIORef', newIORef, readIORef)
import FastString       (FastString)
import Foreign.Storable (Storable)
import GHC.IO.Unsafe    (unsafeDupablePerformIO)
import Outputable       (Outputable)
import System.IO.Unsafe (unsafePerformIO)
import Unique           (Uniquable)



------------------------
-- === FastString === --
------------------------

-- === Missing instances === --

instance Convertible String     FastString where convert = fromString          ; {-# INLINE convert #-}
instance Convertible FastString String     where convert = FastString.unpackFS ; {-# INLINE convert #-}



------------------
-- === Name === --
------------------

-- === Definition === --

newtype Name = Name FastString
    deriving (Binary, Data, Eq, Prelude.Monoid, Ord, Outputable, Uniquable)
makeLenses ''Name


-- === Instances === --

instance Show Name where
    show = show . unwrap ; {-# INLINE show #-}



-----------------
-- === Ref === --
-----------------

-- | The 'Ref' is a reference to name. You can use it to query the Name State
--   for name representation and you can serialize it in binary formats with
--   ease. Currently, it's implemented simply as newtype over 'Int', however
--   do not rely on ninternal representtion.

-- === Definition === ---

newtype Ref = Ref Int deriving (Eq, Storable)
makeLenses ''Ref


-- === Instances === --

instance Convertible Name   Ref where convert = wrap . FastString.uniq . unwrap ; {-# INLINE convert    #-}
instance Convertible String Ref where convert = convertVia @Name                ; {-# INLINE convert    #-}
instance IsString           Ref where fromString = convert                      ; {-# INLINE fromString #-}
instance Show               Ref where show       = show . convertTo @String     ; {-# INLINE show       #-}
instance Num                Ref where
    fromInteger = wrap . fromInteger ; {-# INLINE fromInteger #-}
    (+)         = error "unsupported1"
    (-)         = error "unsupported2"
    (*)         = error "unsupported3"
    abs         = error "unsupported4"
    signum      = error "unsupported5"



--------------------
-- === RefMap === --
--------------------

-- === Definition === --

newtype RefMap = RefMap (IntMap Name)
    deriving (Semigroup, Mempty, Show)
makeLenses ''RefMap


-- === API === --

refMap :: IORef RefMap
refMap = unsafePerformIO $! newIORef mempty
{-# NOINLINE refMap #-}

getRefMap :: MonadIO m => m RefMap
getRefMap = liftIO $! readIORef refMap
{-# NOINLINE getRefMap #-}

registerName :: Name -> Name
registerName !name = out where
    !out = unsafePerformIO $ do
        let nameRef = convert name :: Ref
        atomicModifyIORef' refMap
            $ (,()) . (wrapped %~ IntMap.insert (coerce nameRef) name)
        return name
{-# NOINLINE registerName #-}



-- === Instances === --

instance Convertible Name       FastString where convert = unwrap ; {-# INLINE convert #-}
instance Convertible FastString Name       where
    convert !s = out where
        !out = registerName $! wrap s
    {-# INLINE convert #-}

instance IsString           Name   where fromString = convert                ; {-# INLINE fromString #-}
instance Convertible String Name   where convert    = convertVia @FastString ; {-# INLINE convert #-}
instance Convertible Name   String where convert    = convertVia @FastString ; {-# INLINE convert #-}
instance Convertible Ref    String where convert    = convertVia @Name       ; {-# INLINE convert #-}
instance Convertible Ref    Name   where
    convert !ref = unsafeDupablePerformIO $ do
        map <- getRefMap
        case IntMap.lookup (unwrap ref) (unwrap map) of
            Nothing -> error "Internal error"
            Just n  -> pure n
    {-# NOINLINE convert #-}
