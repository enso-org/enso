{-# LANGUAGE UndecidableInstances #-}

module Foreign.DynamicStorable where

import Prologue

import Foreign.Ptr (Ptr)

class DynamicStorable a where
    sizeOf :: a -> IO Int
    peek   :: Ptr a -> IO a
    poke   :: Ptr a -> a -> IO ()

