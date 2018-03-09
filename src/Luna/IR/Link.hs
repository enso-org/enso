module Luna.IR.Link where

import Prologue

import           OCI.IR.Conversion (generalize)
import qualified OCI.IR.Layout     as Layout
import           OCI.IR.Link



------------------------
-- === Link types === --
------------------------

-- type Term t src = SubLink (Layout.Rebase t src) Layout.Term
-- type Type t src = SubLink (Layout.Rebase t src) Layout.Type
-- type Name t src = SubLink (Layout.Rebase t src) Layout.Name



-----------------------
-- === Instances === --
-----------------------


class HasLinks a where
    readLinksIO :: a -> IO [SomeLink]

readLinks :: (HasLinks a, MonadIO m) => a -> m [SomeLink]
readLinks = liftIO . readLinksIO
{-# INLINE readLinks #-}

instance HasLinks (Link a) where
    readLinksIO l = return . pure $ generalize l ; {-# INLINE readLinksIO #-}

instance HasLinks Int where
    readLinksIO _ = return mempty ; {-# INLINE readLinksIO #-}
