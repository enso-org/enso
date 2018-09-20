{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Text.Span where

import qualified Control.Monad.State.Layered        as State
import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Foreign.Storable.Deriving          as Storable

import Data.Maybe         (isJust)
import Data.Text.Position (Delta)
import Foreign.Storable   (Storable)
import Prologue           hiding (length)


------------------------
-- === SpacedSpan === --
------------------------

-- === Definition === --

data SpacedSpan = SpacedSpan
    { __offset :: !Delta
    , __length :: !Delta
    } deriving (Eq, Ord, Show)
makeLenses          ''SpacedSpan
Storable.derive     ''SpacedSpan
GTraversable.derive ''SpacedSpan

newtype LeftSpacedSpan = LeftSpacedSpan SpacedSpan
    deriving (Eq, Ord, Show, Storable)

newtype RightSpacedSpan = RightSpacedSpan SpacedSpan
    deriving (Eq, Ord, Show, Storable)

makeLenses          ''LeftSpacedSpan
makeLenses          ''RightSpacedSpan
GTraversable.derive ''LeftSpacedSpan
GTraversable.derive ''RightSpacedSpan

class    IsSpacedSpan t               where spacedSpan :: Iso' t SpacedSpan
instance IsSpacedSpan LeftSpacedSpan  where spacedSpan = wrapped
instance IsSpacedSpan RightSpacedSpan where spacedSpan = wrapped


-- === Construction === --

leftSpacedSpan  :: Delta -> Delta -> LeftSpacedSpan
rightSpacedSpan :: Delta -> Delta -> RightSpacedSpan
leftSpacedSpan  = LeftSpacedSpan  .: SpacedSpan ; {-# INLINE leftSpacedSpan #-}
rightSpacedSpan = RightSpacedSpan .: SpacedSpan ; {-# INLINE rightSpacedSpan #-}


-- === Props === --

offset, length :: IsSpacedSpan t => Lens' t Delta
offset = spacedSpan . spacedSpan_offset ; {-# INLINE offset #-}
length = spacedSpan . spacedSpan_length ; {-# INLINE length #-}

asOffsetSpan :: IsSpacedSpan t => t -> t
asOffsetSpan s = s & offset %~ (+ s ^. length)
                   & length .~ 0
{-# INLINE asOffsetSpan #-}

asSolid :: IsSpacedSpan t => t -> t
asSolid s = s & length %~ (+ s ^. offset)
              & offset .~ 0
{-# INLINE asSolid #-}

measure :: IsSpacedSpan t => t -> Delta
measure t = t ^. offset + t ^. length ; {-# INLINE measure #-}

prependAsOffset :: LeftSpacedSpan -> LeftSpacedSpan -> LeftSpacedSpan
prependAsOffset (unwrap -> SpacedSpan off s) (unwrap -> SpacedSpan off' s')
    = wrap $ SpacedSpan (off + s + off') s'


-- === Appendable === --

-- | Append is like Semigroup, but it's "smart". When concatenating left spaced
--   offsets it takes into account if the left argument has any length and if
--   not it only increases the offset of the right argument. Apparently, you
--   don't need this automation often. Consider a list `[,1]` - we've got here 2
--   elements: `Span 1 0 Missing` and `Span 1 1 Number`. We want the body to
--   have `Span 1 2`, but using append we'll get `Span 2 1`.

class Appendable t where
    append :: t -> t -> t

instance (Num s, Ord s) => Appendable LeftSpacedSpan where
    append (unwrap -> SpacedSpan off s) (unwrap -> SpacedSpan off' s')
        = if s > 0 then leftSpacedSpan off (s + off' + s')
                   else leftSpacedSpan (off + off') s'

instance (Num s, Ord s) => Appendable RightSpacedSpan where
    append (unwrap -> SpacedSpan s off) (unwrap -> SpacedSpan s' off')
        = if s' > 0 then rightSpacedSpan (s + off + s') off'
                    else rightSpacedSpan s (off + off')


-- === Instances === --

instance Num s => Default SpacedSpan where def    = SpacedSpan 0 0
instance Num s => Mempty  SpacedSpan where mempty = SpacedSpan 0 0

deriving instance Num s => Default LeftSpacedSpan
deriving instance Num s => Default RightSpacedSpan
deriving instance Num s => Mempty  LeftSpacedSpan
deriving instance Num s => Mempty  RightSpacedSpan

instance (Num s, Ord s) => Semigroup LeftSpacedSpan where
    (unwrap -> SpacedSpan off s) <> (unwrap -> SpacedSpan off' s')
        = wrap $ SpacedSpan off (s + off' + s')
    {-# INLINE (<>) #-}

instance (Num s, Ord s) => Semigroup RightSpacedSpan where
    (unwrap -> SpacedSpan s off) <> (unwrap -> SpacedSpan s' off')
        = wrap $ SpacedSpan (s + off + s') off'
    {-# INLINE (<>) #-}

-- Conversions
instance Convertible SpacedSpan      (Delta,Delta)   where convert (SpacedSpan o l) = (o,l)
instance Convertible (Delta,Delta)   SpacedSpan      where convert (o,l) = (SpacedSpan o l)
instance Convertible SpacedSpan      LeftSpacedSpan  where convert = wrap
instance Convertible SpacedSpan      RightSpacedSpan where convert = wrap
instance Convertible LeftSpacedSpan  SpacedSpan      where convert = unwrap
instance Convertible RightSpacedSpan SpacedSpan      where convert = unwrap
instance Convertible LeftSpacedSpan  (Delta,Delta)   where convert = convertVia @SpacedSpan
instance Convertible RightSpacedSpan (Delta,Delta)   where convert = convertVia @SpacedSpan
instance Convertible (Delta,Delta)   LeftSpacedSpan  where convert = convertVia @SpacedSpan
instance Convertible (Delta,Delta)   RightSpacedSpan where convert = convertVia @SpacedSpan
