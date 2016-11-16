{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Functor.Utils where

import Prelude hiding ((.))
import GHC.Exts (Constraint)

-- Nested fmaps

fmap1 = fmap       ; {-# INLINE fmap1 #-}
fmap2 = fmap.fmap  ; {-# INLINE fmap2 #-}
fmap3 = fmap.fmap2 ; {-# INLINE fmap3 #-}
fmap4 = fmap.fmap3 ; {-# INLINE fmap4 #-}
fmap5 = fmap.fmap4 ; {-# INLINE fmap5 #-}
fmap6 = fmap.fmap5 ; {-# INLINE fmap6 #-}
fmap7 = fmap.fmap6 ; {-# INLINE fmap7 #-}
fmap8 = fmap.fmap7 ; {-# INLINE fmap8 #-}
fmap9 = fmap.fmap8 ; {-# INLINE fmap9 #-}

-- Dots

dot1 = (.)         ; {-# INLINE dot1 #-}
dot2 = dot1 . dot1 ; {-# INLINE dot2 #-}
dot3 = dot1 . dot2 ; {-# INLINE dot3 #-}
dot4 = dot1 . dot3 ; {-# INLINE dot4 #-}
dot5 = dot1 . dot4 ; {-# INLINE dot5 #-}
dot6 = dot1 . dot5 ; {-# INLINE dot6 #-}
dot7 = dot1 . dot6 ; {-# INLINE dot7 #-}
dot8 = dot1 . dot7 ; {-# INLINE dot8 #-}
dot9 = dot1 . dot8 ; {-# INLINE dot9 #-}

-- Operators

infixr 9 ∘
infixr 9 ∘∘
infixr 9 ∘∘∘
infixr 9 ∘∘∘∘
infixr 9 ∘∘∘∘∘
(∘)      = fmap ; {-# INLINE (∘)     #-}
(∘∘)     = dot2 ; {-# INLINE (∘∘)    #-}
(∘∘∘)    = dot3 ; {-# INLINE (∘∘∘)   #-}
(∘∘∘∘)   = dot4 ; {-# INLINE (∘∘∘∘)  #-}
(∘∘∘∘∘)  = dot5 ; {-# INLINE (∘∘∘∘∘) #-}

infixr 9 .
infixr 9 .:
infixr 9 .:.
infixr 9 .::
infixr 9 .::.
infixr 9 .:::
infixr 9 .:::.
infixr 9 .::::
infixr 9 .::::.
(.) :: (Functor f) => (a -> b) -> f a -> f b
(.)      = fmap ; {-# INLINE (.)      #-}
(.:)     = dot2 ; {-# INLINE (.:)     #-}
(.:.)    = dot3 ; {-# INLINE (.:.)    #-}
(.::)    = dot4 ; {-# INLINE (.::)    #-}
(.::.)   = dot5 ; {-# INLINE (.::.)   #-}
(.:::)   = dot6 ; {-# INLINE (.:::)   #-}
(.:::.)  = dot7 ; {-# INLINE (.:::.)  #-}
(.::::)  = dot8 ; {-# INLINE (.::::)  #-}
(.::::.) = dot9 ; {-# INLINE (.::::.) #-}



infixl 4 <∘>
infixl 4 <∘∘>
infixl 4 <∘∘∘>
infixl 4 <∘∘∘∘>
infixl 4 <∘∘∘∘∘>
f <∘>     a = fmap  f ∘     a ; {-# INLINE (<∘>)     #-}
f <∘∘>    a = fmap  f ∘∘    a ; {-# INLINE (<∘∘>)    #-}
f <∘∘∘>   a = fmap  f ∘∘∘   a ; {-# INLINE (<∘∘∘>)   #-}
f <∘∘∘∘>  a = fmap  f ∘∘∘∘  a ; {-# INLINE (<∘∘∘∘>)  #-}
f <∘∘∘∘∘> a = fmap  f ∘∘∘∘∘ a ; {-# INLINE (<∘∘∘∘∘>) #-}



f <<∘>>  a = fmap2 f ∘  a ; {-# INLINE (<<∘>> ) #-}
f <<∘∘>> a = fmap2 f ∘∘ a ; {-# INLINE (<<∘∘>>) #-}

infixl 4 <<$>>
f <<$>> a = fmap f <$> a ; {-# INLINE (<<$>>) #-}


-- nested lenses
-- | following functions are usefull when operating on nested structures with lenses, for example
-- | given function foo :: a -> m (n a) and a lens l :: Lens' x a, we can use
-- | nested l foo to get signature of x -> m (n x)

newtype NestedFunctor m n a = NestedFunctor { fromNestedFunctor :: m (n a)} deriving (Show)
instance (Functor m, Functor n) => Functor (NestedFunctor m n) where fmap f = NestedFunctor . (fmap $ fmap f) . fromNestedFunctor

--nested :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))
nested l f = fromNestedFunctor . l (fmap NestedFunctor f)
{-# INLINE nested #-}


type family Functors lst :: Constraint where
    Functors '[]       = ()
    Functors (f ': fs) = (Functor f, Functors fs)
