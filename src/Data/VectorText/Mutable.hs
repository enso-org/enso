module Data.VectorText.Mutable where

import Prologue hiding (Text)
import           Control.Monad.ST    (runST)
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as Vector
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MVector


------------------
-- === Text === --
------------------

-- === Definition === --

newtype MVectorText s = MVectorText (MVector s Char)
makeLenses ''MVectorText


-- === Creation === --

new :: PrimMonad m => Int -> m (MVectorText (PrimState m))
new i = wrap <$> MVector.new i
