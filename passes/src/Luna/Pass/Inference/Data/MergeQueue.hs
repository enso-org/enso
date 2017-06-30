module Luna.Pass.Inference.Data.MergeQueue where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc

newtype MergeQueue = MergeQueue [Expr Unify]
makeWrapped ''MergeQueue

initMergeQueue :: MonadPassManager m => m ()
initMergeQueue = setAttr (getTypeDesc @MergeQueue) $ MergeQueue def
