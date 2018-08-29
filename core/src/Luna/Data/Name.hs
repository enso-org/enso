module Luna.Data.Name (module Luna.Data.Name, module X) where
import OCI.Data.Name as X

import Prologue hiding (concat)

mixfix :: NonEmpty Name -> Name
mixfix = concat . intersperse "." ; {-# INLINE mixfix #-}

