{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType #-}

module Foreign.Info.ByteSize where

import Prologue hiding (Known)

import qualified Control.Monad.State.Layered as State



----------------------
-- === ByteSize === --
----------------------

-- === Definition === --

newtype ByteSize (a :: k) = ByteSize Int deriving (Show)
makeLenses ''ByteSize


-- === API === --

type Known a = State.Getter (ByteSize a)

get :: ∀ a m. Known a m => m Int
get = unwrap <$> State.get @(ByteSize a) ; {-# INLINE get #-}


-- === Instances === --

instance Mempty (ByteSize a) where mempty = wrap 0 ; {-# INLINE mempty #-}

