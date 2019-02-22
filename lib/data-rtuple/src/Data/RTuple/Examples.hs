module Data.RTuple.Examples where

import Prelude       hiding (index, zip, null, map, init, last, length, take, concat, mapM, reverse)
import Data.RTuple
import Data.Typeable
import Control.Lens  hiding (index, indexed)

data A = A deriving (Show)
data B = B deriving (Show)
data C = C deriving (Show)
data D = D deriving (Show)

main = do
    -- let l1      = append 3 $ append 2 $ append 1 empty
    --     Just l2 = fromList (Proxy :: Proxy 10) [1..]
    --
    -- putStrLn "\n=== l1 ==="
    -- print l1
    -- print $ null   l1
    -- print $ single l1
    -- print $ length l1
    --
    -- putStrLn "\n=== l2 ==="
    -- print l2
    -- print $ reverse l2
    -- print $ take (Proxy :: Proxy 3) l2
    -- print $ l2 & last .~ "ala"
    -- print $ (l2 & init .~ empty :: List '[Int])
    --
    -- print $ zip l1 l2
    --
    -- print $ l2 & indexed (Proxy :: Proxy 5) .~ 7
    --
    -- print $ index (Proxy :: Proxy 0) l2
    --
    -- putStrLn "\n=== type maps ==="
    -- let m1 = empty2
    --     m2 = insert2 (Proxy :: Proxy A) 1
    --        $ insert2 (Proxy :: Proxy B) "ala"
    --        $ m1
    --
    -- print m1
    -- print m2
    -- --print $ access (Proxy :: Proxy A) m2
    -- --print $ access (Proxy :: Proxy A) m2

    print "hello!"
