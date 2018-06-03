module Data.SmallAutoVector.Mutable.Storable
    (module Data.SmallAutoVector.Mutable.Storable, module X) where
import Data.AutoVector.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, fromList, toList,
                 unsafeRead)

import qualified Data.Construction         as Data
import qualified Data.List                 as List
import qualified Foreign.DynamicStorable   as DynamicStorable
import qualified Foreign.Marshal.Alloc     as Mem
import qualified Foreign.Marshal.Utils     as Mem
import qualified Foreign.Storable.Deriving as Storable
import qualified Foreign.Storable.Utils    as Storable
import qualified Type.Known                as Type

import Data.AutoVector.Mutable.Storable (Vector)
import Foreign.DynamicStorable          (DynamicStorable)
import Foreign.Ptr                      (Ptr, nullPtr, plusPtr)
import Foreign.Storable                 (Storable)
import Foreign.Storable.Utils           (castPeekAndOffset, castPokeAndOffset)
import System.IO.Unsafe                 (unsafeDupablePerformIO,
                                         unsafePerformIO)

-------------------------
-- === SmallVector === --
-------------------------

-- === Definition === --

data SmallVector (n :: Nat) a = SmallVector
    { _smallPtr :: !(Ptr a)
    , _largePtr :: !(Vector a)
    }
makeLenses ''SmallVector

type instance Item (SmallVector n a) = a


-- === Instances === --

-- unsafeNewInMemory :: ∀ n a m. KnownNat n => Ptr () -> m (SmallVector n a)
-- unsafeNewInMemory = \ptr -> do
--     let smallSize    = fromIntegral $! Type.from @n
--         bodyByteSize =
