{-# LANGUAGE PolyKinds #-}

module Data.Typeable.Proxy.Abbr where

import Data.Typeable

p :: Proxy a
p = Proxy ; {-# INLINE p #-}

type P a = Proxy a
