module OCI.Data.Name where

import Prologue

import qualified Data.IntMap.Strict    as IntMap
import qualified FastString            as FastString
import qualified Language.Symbol.Label as Label
import qualified Prelude               as Prelude

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
instance Semigroup   FastString            where (<>)    = FastString.appendFS ; {-# INLINE (<>)    #-}



-------------------
-- === Value === --
-------------------

-- === Definition === --

newtype Value = Value FastString
    deriving ( Binary, Data, Eq, Prelude.Monoid, Ord, Outputable, Uniquable )
makeLenses ''Value


-- === API === --

concatValues :: [Value] -> Value
concatValues = wrap . FastString.concatFS . fmap unwrap ; {-# INLINE concatValues #-}


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
makeLenses ''Name


-- === API === --

concat :: [Name] -> Name
concat = convert . concatValues . fmap convert ; {-# INLINE concat #-}

value :: Name -> Value
value = convert ; {-# INLINE value #-}


-- === Instances === --

instance Convertible Value  Name where convert = wrap . FastString.uniq . unwrap ; {-# INLINE convert    #-}
instance Convertible Char   Name where convert = convertVia @String              ; {-# INLINE convert    #-}
instance Convertible String Name where convert = convertVia @Value               ; {-# INLINE convert    #-}
instance Show               Name where show    = show . convertTo @String        ; {-# INLINE show       #-}
instance Semigroup          Name where n <> n' = convert $ value n <> value n'   ; {-# INLINE (<>)       #-}
instance IsString           Name where fromString = convert                      ; {-# INLINE fromString #-}



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
        return name
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
instance Convertible Name   Value  where
    convert !ref = unsafeDupablePerformIO $ do
        map <- getNameMap
        case IntMap.lookup (unwrap ref) (unwrap map) of
            Nothing -> error $ "Panic. Name lookup error for name id"
                    <> show (unwrap ref)
            Just n  -> pure n
    {-# NOINLINE convert #-}

instance Semigroup Value where
    Value v <> Value v' = out where
        !out = convert $ v <> v'
    {-# INLINE (<>) #-}

-- TODO !!!!!!
-- Remove?

instance Label.HasLabel Name
