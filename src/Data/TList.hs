
module Data.TList where

import Prelude hiding (head, tail)
import Control.Lens
import Control.Lens.Utils


-------------------
-- === TList === --
-------------------

-- === Definition === --

newtype TList as = TList (RTuple as)

type family RTuple as where
    RTuple '[]       = ()
    RTuple (a ': as) = (a, RTuple as)

makeWrapped ''TList


-- === Utils === --

empty :: TList '[]
empty = TList () ; {-# INLINE empty #-}

head :: Lens' (TList (a ': as)) a
head = wrapped' . _1 ; {-# INLINE head #-}

tail :: Lens' (TList (a ': as)) (TList as)
tail = wrapped' . _2 . from wrapped' ; {-# INLINE tail #-}


-- Focus
class                        Focus lst       a where focus :: Lens' (TList lst) a
instance {-# OVERLAPPING #-} Focus (a ': ls) a where focus = head         ; {-# INLINE focus #-}
instance Focus ls a       => Focus (l ': ls) a where focus = tail . focus ; {-# INLINE focus #-}
