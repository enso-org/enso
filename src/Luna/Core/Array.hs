{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Luna.Core.Array where

import Prelude

import Control.DeepSeq
import Data.Array.Storable
import Data.Array.Storable.Internals
import GHC.ForeignPtr
import GHC.Generics (Generic)

import Luna.Core.Data


type SArr = StorableArray Int (UniCore ())

deriving instance Generic SArr

instance NFData (ForeignPtr a) where rnf (ForeignPtr !_ !_) = ()

instance NFData  SArr

mkArray :: Int -> IO SArr
mkArray !i = newArray_ (0, i + 1)
{-# INLINE mkArray #-}

mknodes :: Int -> SArr -> IO SArr
mknodes !i arr = do
    let go 0 = return ()
        go j = writeArray arr j (UAcc $ Acc (ULink (Keyx j)) (ULink (Keyx (j + 1)))) >> go (j - 1)
    go i
    return arr
{-# INLINE mknodes #-}
