{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# EXT      InlineAll            #-}

module OCI.IR.Name.Class where

import Luna.Prelude
import qualified Data.FastString as FS
import Data.FastString (FastString, concatFS)
import Data.Data
import Outputable (Outputable)
import Unique
import qualified Language.Symbol.Label as Symbol



------------------
-- === Name === --
------------------

-- === Definitions === --

newtype Name = Name FastString deriving (Generic, NFData, Data, Read, Eq, Typeable, Outputable, Uniquable, Binary, Mempty, Semigroup, IsString, ToString)
makeClassy ''Name


-- === Hashable === --

class HashableName a where
    nameHash :: Lens' a Name


-- === Utils === --

concatNames :: [Name] -> Name
concatNames = wrap . concatFS . fmap unwrap

type IsName   a = (FromName a, ToName a)
type FromName a = Convertible' Name a
type ToName   a = Convertible' a Name


-- === Instances === --

-- Show
instance Show Name where show = show . unwrap

-- Ord
-- | Name's ordering bases on assigned unique numbers
instance Ord Name where compare = compare `on` getUnique

-- Conversions
instance Convertible FastString Name       where convert = wrap
instance Convertible Name       FastString where convert = unwrap
instance Convertible String     Name       where convert = wrap    ∘ convert
instance Convertible Name       String     where convert = convert ∘ unwrap
instance Convertible Text       Name       where convert = convertVia @String
instance Convertible Name       Text       where convert = convertVia @String

-- Symbol identity labels
instance Symbol.HasLabel Name

-- Hashable
instance HashableName Name where nameHash = id
