{-# LANGUAGE TypeInType #-}

module Data.Path where

import Data.Kind

data (a :: k) // (as :: l)
infixr 5 //

type TypeSep = ((//) :: * -> * -> *)
