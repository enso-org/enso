{-# LANGUAGE UndecidableInstances #-}

module OCI.Data.Name.Class where
import OCI.Data.Name.Instances ()

import Prologue hiding (concat)

import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Data.IntMap.Strict                 as IntMap
import qualified FastString                         as FastString
import qualified Foreign.Storable.Class             as Storable
import qualified Language.Symbol.Label              as Label
import qualified Prelude                            as Prelude

import Binary           (Binary)
import Data.IntMap      (IntMap)
import Data.IORef       (IORef, atomicModifyIORef', newIORef, readIORef)
import FastString       (FastString)
import Foreign.Storable (Storable)
import GHC.IO.Unsafe    (unsafeDupablePerformIO)
import Outputable       (Outputable)
import System.IO.Unsafe (unsafePerformIO)
import Unique           (Uniquable)


-------------------
-- === Value === --
-------------------

-- === Definition === --

newtype Value = Value FastString
    deriving ( Binary, Data, Eq, Ord, Outputable, Uniquable )
makeLenses ''Value


-- === API === --

concatValues :: [Value] -> Value
concatValues = convert . FastString.concatFS . fmap unwrap ; {-# INLINE concatValues #-}


-- === Instances === --

instance Show Value where
    show = show . unwrap ; {-# INLINE show #-}



------------------
-- === Name === --
------------------

-- | The 'Name' is a reference to name. You can use it to query the Value State
--   for name representation and you can serialize it in binary formats with
--   ease. Currently, it's implemented simply as newtype over 'Int', however
--   do not rely on ninternal representtion.

-- === Definition === ---

newtype Name = Name Int deriving (Eq, Num, Ord, Storable)
makeLenses          ''Name
GTraversable.derive ''Name


-- === API === --

concat :: [Name] -> Name
concat = convert . concatValues . fmap convert ; {-# INLINE concat #-}

value :: Name -> Value
value = convert ; {-# INLINE value #-}


-- === Instances === --

instance Convertible Value  Name where convert = wrap . FastString.uniq . unwrap ; {-# INLINE convert    #-}
instance Convertible Char   Name where convert = convertVia @String              ; {-# INLINE convert    #-}
instance Convertible String Name where convert = convertVia @Value               ; {-# INLINE convert    #-}
instance Convertible Text   Name where convert = convertVia @String              ; {-# INLINE convert    #-}
instance Show               Name where show    = show . convertTo @String        ; {-# INLINE show       #-}
instance Semigroup          Name where n <> n' = convert $ value n <> value n'   ; {-# INLINE (<>)       #-}
instance IsString           Name where fromString = convert                      ; {-# INLINE fromString #-}

instance MonadIO m => Storable.Peek t m Name
instance MonadIO m => Storable.Poke t m Name
instance Storable.KnownConstantSize Name where
    constantSize = Storable.constantSize @(Unwrapped Name)
    {-# INLINE constantSize #-}


----------------------
-- === NameMap === --
---------------------

-- === Definition === --

newtype NameMap = NameMap (IntMap Value)
    deriving (Semigroup, Mempty, Show)
makeLenses ''NameMap


-- === API === --

nameMap :: IORef NameMap
nameMap = unsafePerformIO $! newIORef mempty
{-# NOINLINE nameMap #-}

getNameMap :: MonadIO m => m NameMap
getNameMap = liftIO $! readIORef nameMap
{-# NOINLINE getNameMap #-}

registerName :: Value -> Value
registerName !name = out where
    !out = unsafePerformIO $ do
        let nameRef = convertTo @Name name
        atomicModifyIORef' nameMap
            $ (,()) . (wrapped %~ IntMap.insert (coerce nameRef) name)
        pure name
{-# NOINLINE registerName #-}



-- === Instances === --

instance Convertible Value      FastString where convert = unwrap ; {-# INLINE convert #-}
instance Convertible FastString Value      where
    convert !s = out where
        !out = registerName $! wrap s
    {-# INLINE convert #-}

instance IsString           Value  where fromString = convert                ; {-# INLINE fromString #-}
instance Convertible Char   Value  where convert    = convertVia @String     ; {-# INLINE convert #-}
instance Convertible String Value  where convert    = convertVia @FastString ; {-# INLINE convert #-}
instance Convertible Value  String where convert    = convertVia @FastString ; {-# INLINE convert #-}
instance Convertible Name   String where convert    = convertVia @Value      ; {-# INLINE convert #-}
instance Convertible Name   Text   where convert    = convertVia @String     ; {-# INLINE convert #-}
instance Convertible Name   Value  where
    convert !ref = unsafeDupablePerformIO $ do
        map <- getNameMap
        case IntMap.lookup (unwrap ref) (unwrap map) of
            Nothing -> error $ "Panic. Name lookup error for name id"
                    <> show (unwrap ref)
            Just n  -> pure n
    {-# NOINLINE convert #-}

deriving instance Prelude.Monoid Value
instance Semigroup Value where
    Value v <> Value v' = out where
        !out = convert $ v <> v'
    {-# INLINE (<>) #-}


-- TODO !!!!!!
-- Remove? vvv
instance Label.HasLabel Name

-- instance ExternalStorable Name
-- instance ExternalFieldStorable Name
