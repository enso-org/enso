{-# LANGUAGE CPP #-}

module Data.Convert.Instances.OverloadedLabels where

import Prelude
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Proxy
import Data.Convert.Class
import Data.Convert.Instances.Text

import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy

#if __GLASGOW_HASKELL__ >= 802
instance KnownSymbol s => IsLabel s Strict.Text where fromLabel = convert $ symbolVal (Proxy :: Proxy s) ; {-# INLINE fromLabel #-}
instance KnownSymbol s => IsLabel s Lazy.Text   where fromLabel = convert $ symbolVal (Proxy :: Proxy s) ; {-# INLINE fromLabel #-}
instance KnownSymbol s => IsLabel s String      where fromLabel =           symbolVal (Proxy :: Proxy s) ; {-# INLINE fromLabel #-}
#else
instance KnownSymbol s => IsLabel s Strict.Text where fromLabel _ = convert $ symbolVal (Proxy :: Proxy s) ; {-# INLINE fromLabel #-}
instance KnownSymbol s => IsLabel s Lazy.Text   where fromLabel _ = convert $ symbolVal (Proxy :: Proxy s) ; {-# INLINE fromLabel #-}
instance KnownSymbol s => IsLabel s String      where fromLabel _ =           symbolVal (Proxy :: Proxy s) ; {-# INLINE fromLabel #-}
#endif
