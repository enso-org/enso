{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Functor.Utils where

import Prelude hiding ((.))
import GHC.Exts (Constraint)
import Data.Traversable (mapM)

-- Nested fmaps

fmap1 = fmap
fmap2 = fmap.fmap
fmap3 = fmap.fmap2
fmap4 = fmap.fmap3
fmap5 = fmap.fmap4
fmap6 = fmap.fmap5
fmap7 = fmap.fmap6
fmap8 = fmap.fmap7
fmap9 = fmap.fmap8

-- Dots

dot1 = (.)
dot2 = dot1 . dot1
dot3 = dot1 . dot2
dot4 = dot1 . dot3
dot5 = dot1 . dot4
dot6 = dot1 . dot5
dot7 = dot1 . dot6
dot8 = dot1 . dot7
dot9 = dot1 . dot8

-- Operators

infixr 9 ∘
infixr 9 ∘∘
infixr 9 ∘∘∘
infixr 9 ∘∘∘∘
infixr 9 ∘∘∘∘∘
(∘)      = fmap
(∘∘)     = dot2
(∘∘∘)    = dot3
(∘∘∘∘)   = dot4
(∘∘∘∘∘)  = dot5

infixr 9 .
infixr 9 .:
infixr 9 .:.
infixr 9 .::
infixr 9 .::.
infixr 9 .:::
infixr 9 .:::.
infixr 9 .::::
infixr 9 .::::.
(.) :: Functor f => (a -> b) -> f a -> f b
(.)      = fmap
(.:)     = dot2
(.:.)    = dot3
(.::)    = dot4
(.::.)   = dot5
(.:::)   = dot6
(.:::.)  = dot7
(.::::)  = dot8
(.::::.) = dot9



infixl 4 <∘>
infixl 4 <∘∘>
infixl 4 <∘∘∘>
infixl 4 <∘∘∘∘>
infixl 4 <∘∘∘∘∘>
f <∘>     a = fmap  f ∘     a
f <∘∘>    a = fmap  f ∘∘    a
f <∘∘∘>   a = fmap  f ∘∘∘   a
f <∘∘∘∘>  a = fmap  f ∘∘∘∘  a
f <∘∘∘∘∘> a = fmap  f ∘∘∘∘∘ a



f <<∘>>  a = fmap2 f ∘  a
f <<∘∘>> a = fmap2 f ∘∘ a

infixl 4 <<$>>
f <<$>> a = fmap f <$> a

infixl 4 <<*>>
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = (<*>) . fmap (<*>)

-- nested lenses
-- | following functions are usefull when operating on nested structures with lenses, for example
-- | given function foo :: a -> m (n a) and a lens l :: Lens' x a, we can use
-- | nested l foo to get signature of x -> m (n x)

newtype NestedFunctor m n a = NestedFunctor { fromNestedFunctor :: m (n a)} deriving (Show)
instance (Functor m, Functor n) => Functor (NestedFunctor m n) where fmap f = NestedFunctor . (fmap $ fmap f) . fromNestedFunctor

--nested :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))
nested l f = fromNestedFunctor . l (fmap NestedFunctor f)


type family Functors lst :: Constraint where
    Functors '[]       = ()
    Functors (f ': fs) = (Functor f, Functors fs)


-- === Monadic interface === --

infixl 4 <$$>
(<$$>) :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
(<$$>) = mapM
