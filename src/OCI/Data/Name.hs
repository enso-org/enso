module OCI.Data.Name where

import Prologue

import qualified Data.IntMap.Strict as IntMap
import qualified FastString         as FastString
import qualified Prelude            as Prelude

import Binary           (Binary)
import Data.IntMap      (IntMap)
import FastString       (FastString)
import Foreign.Storable (Storable)
import Outputable       (Outputable)
import Unique           (Uniquable)


------------------
-- === Name === --
------------------

-- === Definition === --

newtype Name = Name FastString
    deriving ( Binary, Data, Eq, IsString, Prelude.Monoid, Ord, Outputable
             , Show, Uniquable )
makeLenses ''Name



-----------------
-- === Ref === --
-----------------

-- | The 'Ref' is a reference to name. You can use it to query the Name State
--   for name representation and you can serialize it in binary formats with
--   ease. Currently, it's implemented simply as newtype over 'Int', however
--   do not rely on ninternal representtion.

-- === Definition === ---

newtype Ref = Ref Int deriving (Eq, Show, Storable)
makeLenses ''Ref


-- === Instances === --

instance Num Ref where
    fromInteger = wrap . fromInteger ; {-# INLINE fromInteger #-}
    (+)         = error "unsupported1"
    (-)         = error "unsupported2"
    (*)         = error "unsupported3"
    abs         = error "unsupported4"
    signum      = error "unsupported5"

instance IsString Ref where
    fromString = wrap . FastString.uniq . fromString ; {-# INLINE fromString #-}



--------------------
-- === RefMap === --
--------------------

-- === Definition === --

newtype RefMap = RefMap (IntMap Name) deriving (Show)
