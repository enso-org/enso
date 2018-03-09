module Luna.IR.Link where

import Foreign          (Ptr)
import Foreign.Storable (Storable)
import Prologue

import Luna.IR.Term

import qualified Data.Tag as Tag

import Foreign                (castPtr)
import Foreign.Storable.Utils
import OCI.IR.Component
import OCI.IR.Conversion      (generalize)
import Type.Data.Ord



-- type family IRDef a


------------------
-- === Link === --
------------------

-- === Definition === ---

componentInstance "Link"
type SomeLink = Link ()


data src :-: tgt


--------------------
-- === Layers === --
--------------------

data Source
data Target

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT




----------------------
-- === HasLinks === --
----------------------


class HasLinks a where
    readLinksIO :: a -> IO [SomeLink]

readLinks :: (HasLinks a, MonadIO m) => a -> m [SomeLink]
readLinks = liftIO . readLinksIO
{-# INLINE readLinks #-}

instance HasLinks (Link a) where
    readLinksIO l = pure . pure $ generalize l ; {-# INLINE readLinksIO #-}

instance HasLinks Int where
    readLinksIO _ = pure mempty ; {-# INLINE readLinksIO #-}
