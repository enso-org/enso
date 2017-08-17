{-# EXT InlineAll #-}

module Data.Container.Mono.Class (module Data.Container.Mono.Class, module X) where

import Prologue_old hiding (empty)
import Data.Container.Class as X (Item)

-- type family Item a -- for now defined in Data.Container
type family Key a

-- === Construction === --

class Empty a where
    {-# MINIMAL #-}
    empty :: a
    default empty :: Monoid a => a
    empty = mempty

class Singleton a where
    {-# MINIMAL #-}
    singleton :: Item a -> a
    default singleton :: (Empty a, Appendable a) => Item a -> a
    singleton a = append a empty


-- === Appednables & Prependables === --

class Appendable a where
    {-# MINIMAL append | appendMany #-}
    append     ::                  Item a  -> a -> a
    appendMany :: Foldable f => f (Item a) -> a -> a
    append     = appendMany . (:[])
    appendMany = flip $ foldl (flip append)

class Prependable a where
    {-# MINIMAL prepend | prependMany #-}
    prepend     ::                  Item a  -> a -> a
    prependMany :: Foldable f => f (Item a) -> a -> a
    prepend     = prependMany . (:[])
    prependMany = flip $ foldl (flip prepend)

infixl 5 |>
infixr 5 <|
(|>) :: Appendable  a => a -> Item a -> a
(<|) :: Prependable a => Item a -> a -> a
(|>) = flip append
(<|) = prepend

infixl 5 |>>
infixr 5 <<|
(|>>) :: (Appendable  a, Foldable f) => a -> f (Item a) -> a
(<<|) :: (Prependable a, Foldable f) => f (Item a) -> a -> a
(|>>) = flip appendMany
(<<|) = prependMany


-- === Instances === --

instance Prependable [a] where prepend = (:)
