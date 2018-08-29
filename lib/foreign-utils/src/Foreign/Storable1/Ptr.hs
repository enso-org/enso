{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Foreign.Storable1.Ptr where

import Control.Monad.IO.Class
import Prelude

import qualified Foreign.Storable1 as Storable1

import Foreign               (Ptr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable1     (Storable1)


malloc :: ∀ t a m. (MonadIO m, Storable1 t) => m (Ptr (t a))
malloc  = liftIO . mallocBytes $ Storable1.sizeOf' @t ; {-# INLINE malloc #-}

new :: (MonadIO m, Storable1 t) => t a -> m (Ptr (t a))
new val = liftIO $ do
    ptr <- malloc
    Storable1.poke ptr val
    return ptr
{-# INLINE new #-}

