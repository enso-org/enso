module Luna.Pass.Inference.Data.SimplifierQueue where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc

newtype SimplifierQueue = SimplifierQueue [Expr Monadic]
makeWrapped ''SimplifierQueue

initSimplifierQueue :: MonadPassManager m => m ()
initSimplifierQueue = setAttr (getTypeDesc @SimplifierQueue) $ SimplifierQueue def
