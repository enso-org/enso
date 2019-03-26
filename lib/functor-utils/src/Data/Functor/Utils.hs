{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Functor.Utils (module Data.Functor.Utils, module X) where

import Prelude hiding ((.))
import GHC.Exts (Constraint)
import Data.Functor.Compose as X (Compose(Compose), getCompose)
import Control.Lens


-- === Multi-constraint type families === --

type family Functors lst :: Constraint where
    Functors '[]       = ()
    Functors (f ': fs) = (Functor f, Functors fs)

type family Applicatives lst :: Constraint where
    Applicatives '[]       = ()
    Applicatives (f ': fs) = (Applicative f, Applicatives fs)


-- === Nested fmaps === --

fmap0                                  ::                               (a -> b) ->                    a     ->                    b
(.)                          , (∘)     :: Functor  f1                => (a -> b) ->                 f1 a     ->                 f1 b
fmap2, (.:)  , (<<$>>)       , (∘∘)    :: Functors '[f1,f2]          => (a -> b) ->             f2 (f1 a)    ->             f2 (f1 b)
fmap3, (.:.) , (<<<$>>>)     , (∘∘∘)   :: Functors '[f1,f2,f3]       => (a -> b) ->         f3 (f2 (f1 a))   ->         f3 (f2 (f1 b))
fmap4, (.::) , (<<<<$>>>>)   , (∘∘∘∘)  :: Functors '[f1,f2,f3,f4]    => (a -> b) ->     f4 (f3 (f2 (f1 a)))  ->     f4 (f3 (f2 (f1 b)))
fmap5, (.::.), (<<<<<$>>>>>) , (∘∘∘∘∘) :: Functors '[f1,f2,f3,f4,f5] => (a -> b) -> f5 (f4 (f3 (f2 (f1 a)))) -> f5 (f4 (f3 (f2 (f1 b))))

fmap0 = ($)        ; {-# INLINE fmap0 #-}
fmap1 = fmap       ; {-# INLINE fmap1 #-}
fmap2 = fmap.fmap  ; {-# INLINE fmap2 #-}
fmap3 = fmap.fmap2 ; {-# INLINE fmap3 #-}
fmap4 = fmap.fmap3 ; {-# INLINE fmap4 #-}
fmap5 = fmap.fmap4 ; {-# INLINE fmap5 #-}

-- === Dot operators === --

infixr 9 .
infixr 8 .:
infixr 8 .:.
infixr 8 .::
infixr 8 .::.
(.)      = fmap  ; {-# INLINE (.)      #-}
(.:)     = fmap2 ; {-# INLINE (.:)     #-}
(.:.)    = fmap3 ; {-# INLINE (.:.)    #-}
(.::)    = fmap4 ; {-# INLINE (.::)    #-}
(.::.)   = fmap5 ; {-# INLINE (.::.)   #-}

-- === UTF8 operators === --

infixr 9 ∘
infixr 8 ∘∘
infixr 8 ∘∘∘
infixr 8 ∘∘∘∘
infixr 8 ∘∘∘∘∘
(∘)      = fmap  ; {-# INLINE (∘)     #-}
(∘∘)     = fmap2 ; {-# INLINE (∘∘)    #-}
(∘∘∘)    = fmap3 ; {-# INLINE (∘∘∘)   #-}
(∘∘∘∘)   = fmap4 ; {-# INLINE (∘∘∘∘)  #-}
(∘∘∘∘∘)  = fmap5 ; {-# INLINE (∘∘∘∘∘) #-}

-- === Applicative operators === --

infixl 4 <<$>>
infixl 4 <<<$>>>
infixl 4 <<<<$>>>>
infixl 4 <<<<<$>>>>>
(<<$>>)       = fmap2 ; {-# INLINE (<<$>>)       #-}
(<<<$>>>)     = fmap3 ; {-# INLINE (<<<$>>>)     #-}
(<<<<$>>>>)   = fmap4 ; {-# INLINE (<<<<$>>>>)   #-}
(<<<<<$>>>>>) = fmap5 ; {-# INLINE (<<<<<$>>>>>) #-}

infixl 4 <<*>>
infixl 4 <<<*>>>
infixl 4 <<<<*>>>>
infixl 4 <<<<<*>>>>>
(<<*>>)       :: Applicatives '[f1, f2]             =>             f2 (f1 (a -> b))    ->             f2 (f1 a)    ->             f2 (f1 b)
(<<<*>>>)     :: Applicatives '[f1, f2, f3]         =>         f3 (f2 (f1 (a -> b)))   ->         f3 (f2 (f1 a))   ->         f3 (f2 (f1 b))
(<<<<*>>>>)   :: Applicatives '[f1, f2, f3, f4]     =>     f4 (f3 (f2 (f1 (a -> b))))  ->     f4 (f3 (f2 (f1 a)))  ->     f4 (f3 (f2 (f1 b)))
(<<<<<*>>>>>) :: Applicatives '[f1, f2, f3, f4, f5] => f5 (f4 (f3 (f2 (f1 (a -> b))))) -> f5 (f4 (f3 (f2 (f1 a)))) -> f5 (f4 (f3 (f2 (f1 b))))
(<<*>>)       = (<*>) . fmap (<*>)       ; {-# INLINE (<<*>>)       #-}
(<<<*>>>)     = (<*>) . fmap (<<*>>)     ; {-# INLINE (<<<*>>>)     #-}
(<<<<*>>>>)   = (<*>) . fmap (<<<*>>>)   ; {-# INLINE (<<<<*>>>>)   #-}
(<<<<<*>>>>>) = (<*>) . fmap (<<<<*>>>>) ; {-# INLINE (<<<<<*>>>>>) #-}


-- === Functors remembering call args === --

infixl 4 |$
infixl 4 $|
(|$) :: (a -> b) -> a -> (a, b)
($|) :: (a -> b) -> a -> (b, a)
f |$ a = (a, f a) ; {-# INLINE (|$) #-}
f $| a = (f a, a) ; {-# INLINE ($|) #-}

infixl 4 <|$>
infixl 4 <$|>
(<|$>) :: Functor f => (a -> b) -> f a -> f (a, b)
(<$|>) :: Functor f => (a -> b) -> f a -> f (b, a)
f <|$> a = (f |$) <$> a ; {-# INLINE (<|$>) #-}
f <$|> a = (f $|) <$> a ; {-# INLINE (<$|>) #-}


-- === Functor composition === --

composed :: Iso' (f (g a)) (Compose f g a)
composed = iso Compose getCompose ; {-# INLINE composed #-}


-- FIXME[WD]: Think if lenses doesnt provide any counterpart to it.
-- === Nested functors === --

-- nested lenses
-- | following functions are usefull when operating on nested structures with lenses, for example
-- | given function foo :: a -> m (n a) and a lens l :: Lens' x a, we can use
-- | nested l foo to get signature of x -> m (n x)

-- nested :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))
-- nested :: (Functor f10, Functor f0) => (f0 (Compose m1 n1 a1) -> f10 (Compose m n a)) -> f0 (m1 (n1 a1)) -> f10 (m (n a))
nested l f = getCompose . l (fmap Compose f) ; {-# INLINE nested #-}
