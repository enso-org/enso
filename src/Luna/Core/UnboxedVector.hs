module Luna.Core.UnboxedVector where

import           Prelude

import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Unboxed.Mutable  as Vector
import           Data.Vector.Unboxed.Mutable  (IOVector)

import Luna.Core.Data


-- derivingUnbox "UniCore"
--     [t| forall a. UniCore a -> (Bool, Int, Int)                                                                         |]
--     [|  \x -> case x of UVar (Var n) -> (True, n, 0); UAcc (Acc (ULink (EdgeID n1)) (ULink (EdgeID n2))) -> (False, n1, n2) |]
--     [|  \(b, n1, n2) -> if b then UVar (Var n1) else UAcc (Acc (ULink (EdgeID n1)) (ULink (EdgeID n2)))                     |]
--
--
-- type Vec = IOVector (UniCore ())
--
-- mkVec :: Int -> IO Vec
-- mkVec !i = Vector.unsafeNew (i + 1)
--
-- mknodes :: Int -> Vec -> IO Vec
-- mknodes !i !v = do
--     let go 0 = return ()
--         go j = Vector.unsafeWrite v j (UAcc $ Acc (ULink (EdgeID j)) (ULink (EdgeID (j + 1)))) >> go (j - 1)
--     go i
--     return v
