{-# LANGUAGE TypeInType #-}

module Data.Mutable where

import Prologue

import Foreign.Ptr      (Ptr)
import Foreign.Storable (Storable, peekByteOff)



-------------------------------
-- === Mutable Data Type === --
-------------------------------

-- === Definition === --

newtype MData a = MData (Ptr())
makeLenses ''MData

newtype FieldOffset = FieldOffset Int deriving (Show)
makeLenses ''FieldOffset

type family FieldData a field


readField :: forall field a m. (MonadIO m, Storable (FieldData a field))
          => MData a -> FieldOffset -> m (FieldData a field)
readField a off = liftIO $ peekByteOff (unwrap a) (unwrap off) ; {-# INLINE readField #-}
