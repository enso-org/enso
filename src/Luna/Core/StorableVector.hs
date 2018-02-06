{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
module Luna.Core.StorableVector where

import Prelude

import qualified Data.Vector.Storable as Vector hiding (length)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector, IOVector, STVector)

import Luna.Core.Data
import Luna.Core.Store


mknodes :: Int -> Vector (UniCore ()) -> IO (Vector (UniCore ()))
mknodes !i !v = do
    nodes <- Vector.unsafeThaw v
    let go 0 = return ()
        go j = Vector.unsafeWrite nodes j (UAcc $ Acc (ULink (Keyx j)) (ULink (Keyx (j + 1)))) >> go (j - 1)
    go i
    Vector.unsafeFreeze nodes

mknodesU :: Int -> IOVector (UniCore ()) -> IO (IOVector (UniCore ()))
mknodesU !i !v = do
    let go 0 = return ()
        go j = Vector.unsafeWrite v j (UAcc $ Acc (ULink (Keyx j)) (ULink (Keyx (j + 1)))) >> go (j - 1)
    go i
    return v

mknodes2 :: Int -> StoreM' IO (UniCore ()) -> IO ()
mknodes2 !i !s = do
    let go 0 _  = return ()
        go j !v = do
            (v', (k :: Int)) <- reserveKey v
            unsafeWriteSpec v k (Acc (ULink (Keyx j)) (ULink (Keyx (j + 1))) :: Acc ())
            go (j - 1) v
    go i s
    return ()


mkVec :: Int -> IO (Vector (UniCore ()))
mkVec !i = Vector.unsafeFreeze =<< Vector.new (i + 1)


mkVecU :: Int -> IO (IOVector (UniCore ()))
mkVecU !i = Vector.unsafeNew (i + 1)
