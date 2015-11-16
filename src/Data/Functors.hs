{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Functors where

import Prelude hiding ((.))

fmap2 = fmap.fmap
fmap3 = fmap.fmap2
fmap4 = fmap.fmap3
fmap5 = fmap.fmap4
fmap6 = fmap.fmap5
fmap7 = fmap.fmap6
fmap8 = fmap.fmap7
fmap9 = fmap.fmap8


dot1 = (.)
dot2 = dot1 . dot1
dot3 = dot1 . dot2
dot4 = dot1 . dot3
dot5 = dot1 . dot4
dot6 = dot1 . dot5
dot7 = dot1 . dot6
dot8 = dot1 . dot7
dot9 = dot1 . dot8

infixr 9 .
(.) :: (Functor f) => (a -> b) -> f a -> f b
(.)      = fmap
(.:)     = dot2
(.:.)    = dot3
(.::)    = dot4
(.::.)   = dot5
(.:::)   = dot6
(.:::.)  = dot7
(.::::)  = dot8
(.::::.) = dot9