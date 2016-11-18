{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Name.Class where

import Prelude.Luna
import Luna.IR.Name.FastString
import Data.Data
import Outputable (Outputable)
import Unique
import Binary


------------------
-- === Name === --
------------------

newtype Name = Name FastString deriving (Generic, NFData, Data, Show, Read, Eq, Ord, Typeable, Outputable, Uniquable, Binary, Monoid, IsString, ToString, Repr s)
makeWrapped ''Name

class HasName    a where name    :: Lens' a Name
class HasOptName a where optName :: Lens' a (Maybe Name)


-- === Instances === --

-- Basic
instance HasName Name where name = id ; {-# INLINE name #-}

-- Conversions
instance Convertible String Name   where convert = wrap'   ∘ convert ; {-# INLINE convert #-}
instance Convertible Name   String where convert = convert ∘ unwrap' ; {-# INLINE convert #-}



-----------------------
-- === MultiName === --
-----------------------

---- TODO[WD]: make the implementation faster - we can use the same technique as the one used to implement FastString here
data MultiName = MultiName { __base_ :: !Segment, __segs_ :: ![Segment] } deriving (Generic, Show, Read, Eq, Ord)
data Segment   = Segment   { __anum_ :: !Int    , __name_ :: !Name      } deriving (Generic, Show, Read, Eq, Ord)

class HasMultiName    a where multiName    :: Lens' a MultiName
class HasOptMultiName a where optMultiName :: Lens' a (Maybe MultiName)


-- === Instances === --

-- Normal Form
instance NFData MultiName
instance NFData Segment

-- Basic
instance HasMultiName MultiName where multiName = id ; {-# INLINE multiName #-}

-- Strings
instance IsString MultiName where fromString = flip MultiName mempty ∘ fromString ; {-# INLINE fromString #-}
instance IsString Segment   where fromString = Segment 0 ∘ fromString             ; {-# INLINE fromString #-}

instance ToString MultiName where toString (MultiName base segs) = concat $ toString <$> (base : segs) ; {-# INLINE toString #-}
instance ToString Segment   where toString (Segment   anum name) = replicate anum '_' <> toString name ; {-# INLINE toString #-}

-- Repr
instance Repr s MultiName where repr = const "<multiname repr>" ; {-# INLINE repr #-}

