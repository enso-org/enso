module Main where

import Luna.Prelude
import Criterion.Main
import Luna.Test.Utils
import Luna.Test.IR.Runner
import Luna.IR
import qualified OCI.Pass           as Pass

addManyNodes :: Int -> IO Int
addManyNodes amount = do
    Right res <- runPM False $ do
        runRegs
        Pass.eval' @TestPass $ do
            forM_ [0..amount] $ \i -> var (convert $ show i)
            length <$> links
    return res

main = defaultMain $
    [ bgroup "adding multiple nodes" $
        [ bench "1"    $ nfIO $ addManyNodes 1
        , bench "10"   $ nfIO $ addManyNodes 10
        , bench "100"  $ nfIO $ addManyNodes 100
        ]
    ]
