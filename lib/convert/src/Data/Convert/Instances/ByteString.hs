{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Convert.Instances.ByteString where

import Prelude

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import Data.Convert.Class

instance Convertible Strict.ByteString String            where convert = UTF8.toString                 ; {-# INLINE convert #-}
instance Convertible String            Strict.ByteString where convert = UTF8.fromString               ; {-# INLINE convert #-}
instance Convertible Lazy.ByteString   Strict.ByteString where convert = Lazy.toStrict                 ; {-# INLINE convert #-}
instance Convertible Strict.ByteString Lazy.ByteString   where convert = Lazy.fromStrict               ; {-# INLINE convert #-}
instance Convertible Lazy.ByteString   String            where convert = convertVia @Strict.ByteString ; {-# INLINE convert #-}
instance Convertible String            Lazy.ByteString   where convert = convertVia @Strict.ByteString ; {-# INLINE convert #-}

