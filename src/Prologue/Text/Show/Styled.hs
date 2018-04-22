{-# LANGUAGE AllowAmbiguousTypes #-}

module Prologue.Text.Show.Styled where

import Prelude

import Data.Text (Text)



-------------------
-- === Class === --
-------------------

-- === Definition === --

type family StyledShowOutput style

class StyledShow  style a where 
    styledShow :: style -> a -> StyledShowOutput style

class StyledShow1 style a where
    styledShow1 :: ∀ t1. style -> a t1 -> StyledShowOutput style

class StyledShow2 style a where
    styledShow2 :: ∀ t1 t2. style -> a t1 t2 -> StyledShowOutput style


-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} StyledShow1 style a 
    => StyledShow style (a t) where
    styledShow = styledShow1 ; {-# INLINE styledShow #-}

instance {-# OVERLAPPABLE #-} StyledShow2 style a 
    => StyledShow1 style (a t) where
    styledShow1 = styledShow2 ; {-# INLINE styledShow1 #-}



--------------------
-- === Styles === --
--------------------

-- === Pretty === --

data Pretty = Pretty deriving (Show)
type PrettyShow  = StyledShow  Pretty
type PrettyShow1 = StyledShow1 Pretty
type instance StyledShowOutput Pretty = Text

prettyShow  :: PrettyShow  a => a    -> Text
prettyShow1 :: PrettyShow1 a => a t1 -> Text
prettyShow  = styledShow  Pretty ; {-# INLINE prettyShow  #-}
prettyShow1 = styledShow1 Pretty ; {-# INLINE prettyShow1 #-}
