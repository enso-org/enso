{-# LANGUAGE PolyKinds #-}

module Data.Typeable.Proxy.Abbr where

import Data.Typeable

p :: Proxy a
p = Proxy

type P a = Proxy a