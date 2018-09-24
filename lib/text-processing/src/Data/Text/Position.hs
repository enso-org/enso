{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Text.Position where

import Prologue hiding (range)

import qualified Control.Monad.State.Layered        as State
import qualified Data.Generics.Traversable.Deriving as GTraversable

import Data.Bits
import Foreign.Storable
import Text.Printf



-------------------
-- === Delta === --
-------------------

-- | General text position difference descriptor.

-- === Definition === --

newtype Delta = Delta Int
    deriving ( Bits, Bounded, Data, Enum, Eq, FiniteBits, Generic, Integral, Ix
             , NFData, Num, Ord, PrintfArg, Read, Real, Storable)
makeLenses          ''Delta
GTraversable.derive ''Delta


-- === Instances === --

instance Convertible Int   Delta where convert = coerce ; {-# INLINE convert #-}
instance Convertible Delta Int   where convert = coerce ; {-# INLINE convert #-}

instance Default   Delta where def    = 0   ; {-# INLINE def    #-}
instance Mempty    Delta where mempty = 0   ; {-# INLINE mempty #-}
instance Semigroup Delta where (<>)   = (+) ; {-# INLINE (<>)   #-}

instance Show Delta where show = show . unwrap ; {-# INLINE show #-}



--------------------
-- === Offset === --
--------------------

-- | File position as character offset from a control point (like the beginning of the file)

-- === Definition === --

newtype Offset = Offset Delta
    deriving ( Bits, Bounded, Data, Default, Enum, Eq, FiniteBits, Integral, Ix
             , Mempty, NFData, Num, Ord, PrintfArg, Read, Real, Semigroup, Show
             , Storable)
makeLenses ''Offset


-- === Utils === --

succOffset :: State.Monad Offset m => m ()
succOffset = incOffset 1

incOffset :: State.Monad Offset m => Offset -> m ()
incOffset i = State.modify_ @Offset (+i)


-- === Instances === --

instance Convertible Delta Offset where
    convert = coerce
    {-# INLINE convert #-}

instance Convertible Offset Delta where
    convert = coerce
    {-# INLINE convert #-}

instance Convertible Int Offset where
    convert = convertVia @Delta
    {-# INLINE convert #-}

instance Convertible Offset Int where
    convert = convertVia @Delta
    {-# INLINE convert #-}


---------------------
-- === Shifted === --
---------------------

data Shifted a = Shifted { _offset :: Delta
                         , __elem  :: a
                         } deriving (Show, Functor, Traversable, Foldable)
makeLenses ''Shifted

-- instance HasOffset (Shifted a) where offset = shifted_offset


----------------------
-- === Position === --
----------------------

-- | Line / column text position descriptor.

-- === Definition === --

data Position = Position { _line   :: !Delta
                         , _column :: !Delta
                         } deriving (Show, Eq, Data)
makeClassy ''Position


-- === Utils === --

getLine :: State.Getter Position m => m Delta
getLine = view line <$> State.get @Position

getColumn :: State.Getter Position m => m Delta
getColumn = view column <$> State.get @Position

modColumn :: State.Monad Position m => (Delta -> Delta) -> m ()
modColumn f = State.modify_ @Position $ column %~ f

incColumn :: State.Monad Position m => Delta -> m ()
incColumn = modColumn . (+)

succColumn :: State.Monad Position m => m ()
succColumn = modColumn succ

succLine :: State.Monad Position m => m ()
succLine = State.modify_ @Position $ (column .~ mempty) . (line %~ succ)


-- === Instances === --

instance Mempty    Position where mempty                         = def
instance Default   Position where def                            = Position def def
instance Semigroup Position where Position l c <> Position l' c' = Position (l <> l') (c <> c')



-------------------
-- === Range === --
-------------------

-- === Definition === --

data Range = Range { _start :: !Delta
                   , _end   :: !Delta
                   } deriving (Show, Eq, Data)
makeClassy ''Range


-- === Instances === --

instance Default   Range where def                    = Range def def
instance Mempty    Range where mempty                 = Range mempty mempty
instance Semigroup Range where Range s _ <> Range _ e = Range s e



-----------------------
-- === FileOffset == --
-----------------------

-- === Definition === --

newtype FileOffset = FileOffset Delta deriving (Bits, Bounded, Data, Default, Enum, Eq, FiniteBits, Integral, Ix, Mempty, Num, Ord, PrintfArg, Read, Real, Semigroup, Show, Storable)
makeLenses ''FileOffset


-- === Instances === --

instance Convertible Delta FileOffset where convert = wrap
instance Convertible FileOffset Delta where convert = unwrap
instance Convertible Int   FileOffset where convert = convertVia @Delta
instance Convertible FileOffset Int   where convert = convertVia @Delta
