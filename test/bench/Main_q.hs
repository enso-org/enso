module Main_q where

import Prelude
import Control.Monad
import qualified Data.Vector.Storable.Mutable as Vector
import qualified Data.Vector.Storable         as Vector
import           Data.Vector.Storable         (Vector)
import           Criterion.Main


testf :: Int -> Vector Int -> IO (Vector Int)
testf !i !v = do
    mv <- Vector.unsafeThaw v
    let go j = do
          x <- if j == 0 then return 0 else Vector.unsafeRead mv (j - 1)
          Vector.unsafeWrite mv j (x+1)
          when (j < i - 1) $ go (j + 1)
    go 0
    Vector.unsafeFreeze mv

mkVec :: Int -> IO (Vector Int)
mkVec !i = Vector.unsafeFreeze =<< Vector.new (i + 1)

main :: IO ()
main = do
    defaultMain
        [ bgroup "env per all runs"
            $ (\(i :: Int) -> env (mkVec (10 ^ i))
            $ \v -> bench ("10e" ++ show i)
            $ nfIO (testf (10 ^ i) v))  <$> [7..8]

        , bgroup "env per every run"
            $ (\(i :: Int) -> bench ("10e" ++ show i)
            $ perRunEnv (mkVec (10 ^ i))
            $ (testf (10 ^ i)))  <$> [7..8]
        ]
