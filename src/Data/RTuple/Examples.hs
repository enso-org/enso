module Data.RTuple.Examples where

import Prelude       hiding (index, zip, null, map, init, last, length, take, concat, mapM, reverse)
import Data.RTuple
import Data.Typeable
import Control.Lens  hiding (index)

main = do
    let l1      = append 3 $ append 2 $ append 1 empty
        Just l2 = fromList (Proxy :: Proxy 10) [1..]

    putStrLn "\n=== l1 ==="
    print l1
    print $ null   l1
    print $ single l1
    print $ length l1

    putStrLn "\n=== l2 ==="
    print l2
    print $ reverse l2
    print $ take (Proxy :: Proxy 3) l2
    print $ l2 & last .~ "ala"
    print $ (l2 & init .~ empty :: List '[Int])

    print $ zip l1 l2

    print $ index (Proxy :: Proxy 0) l2
    print "hello!"
