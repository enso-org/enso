{-# LANGUAGE PolyKinds #-}

module Data.Container.Type where

import Prelude

--type family In (a :: a) (cont :: c) :: Bool

--type instance In a '[] = False
--type instance In a '[] = False

type family In a lst where
    In a (a ': ls) = True
    In a (l ': ls) = In a ls
    In a '[]       = False