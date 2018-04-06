{-# LANGUAGE UndecidableInstances #-}
{-# EXT InlineAll #-}


module Language.Symbol (module Language.Symbol, module X) where

import Prologue hiding (Symbol)

import Language.Symbol.Label     as X



---------------------
-- === Symbols === --
---------------------

-- === Definition === --

newtype Symbol t s a = Symbol (Definition t s a)
type family Definition t s a
makeLenses ''Symbol


-- === Symbol types === --

data Atom
data Prefix
data Suffix
data Infix
data Mixfix


-- === Utils === --

type FromSymbol t s a p = Convertible (Symbol t s a) p

symbol' :: forall t s a. Definition t s a -> Symbol t s a
symbol' = Symbol

symbol :: forall t s a p. FromSymbol t s a p => Definition t s a -> p
symbol = convert . symbol' @t @s @a

atom'   :: forall t a. Definition t Atom   a -> Symbol t Atom   a
prefix' :: forall t a. Definition t Prefix a -> Symbol t Prefix a
suffix' :: forall t a. Definition t Suffix a -> Symbol t Suffix a
infixx' :: forall t a. Definition t Infix  a -> Symbol t Infix  a
mixfix' :: forall t a. Definition t Mixfix a -> Symbol t Mixfix a
atom'   = Symbol
prefix' = Symbol
suffix' = Symbol
infixx' = Symbol
mixfix' = Symbol

atom   :: forall t a p. FromSymbol t Atom   a p => Definition t Atom   a -> p
prefix :: forall t a p. FromSymbol t Prefix a p => Definition t Prefix a -> p
suffix :: forall t a p. FromSymbol t Suffix a p => Definition t Suffix a -> p
infixx :: forall t a p. FromSymbol t Infix  a p => Definition t Infix  a -> p
mixfix :: forall t a p. FromSymbol t Mixfix a p => Definition t Mixfix a -> p
atom   = convert . atom'   @t @a
prefix = convert . prefix' @t @a
suffix = convert . suffix' @t @a
infixx = convert . infixx' @t @a
mixfix = convert . mixfix' @t @a


-- === Instances === --

deriving instance Show (Definition t s a) => Show (Symbol t s a)

instance (s ~ s', t ~ t', a ~ a')
      => Convertible (Symbol t s a) (Symbol t' s' a') where convert = id


-- === Basic symbol types === --

data Expr
type ExprSymbol = Symbol Expr
type instance Definition Expr Atom   a = a
type instance Definition Expr Prefix a = a -> a
type instance Definition Expr Suffix a = a -> a
type instance Definition Expr Infix  a = a -> a -> a

data Phantom
type PhantomSymbol = Symbol Phantom
type instance Definition Phantom Atom   a = a
type instance Definition Phantom Prefix a = a
type instance Definition Phantom Suffix a = a
type instance Definition Phantom Infix  a = a
type instance Definition Phantom Mixfix a = a



------------------------
-- === Properties === --
------------------------

-- === Body === --

class HasBody a where
    type family BodyOf a
    body :: Lens' a (BodyOf a)

instance HasBody (Symbol t s a) where
    type BodyOf  (Symbol t s a) = Definition t s a
    body = wrapped

instance HasBody a => HasBody (Labeled l a) where
    type BodyOf (Labeled l a) = BodyOf a
    body = labeledContent . body



-----------------------
-- === UniSymbol === --
-----------------------

-- === Definitions === --

data UniSymbol t a = Atom   !(Symbol t Atom   a)
                   | Prefix !(Symbol t Prefix a)
                   | Suffix !(Symbol t Suffix a)
                   | Infix  !(Symbol t Infix  a)
                   | Mixfix !(Symbol t Mixfix a)


-- === Constructors === --

instance (t ~ t', a ~ a') => Convertible (Symbol t Atom   a) (UniSymbol t' a') where convert = Atom
instance (t ~ t', a ~ a') => Convertible (Symbol t Prefix a) (UniSymbol t' a') where convert = Prefix
instance (t ~ t', a ~ a') => Convertible (Symbol t Suffix a) (UniSymbol t' a') where convert = Suffix
instance (t ~ t', a ~ a') => Convertible (Symbol t Infix  a) (UniSymbol t' a') where convert = Infix
instance (t ~ t', a ~ a') => Convertible (Symbol t Mixfix a) (UniSymbol t' a') where convert = Mixfix


-- === Instances === --

deriving instance ( Show (Definition t Atom   a)
                  , Show (Definition t Prefix a)
                  , Show (Definition t Suffix a)
                  , Show (Definition t Infix  a)
                  , Show (Definition t Mixfix a)
                  ) => Show (UniSymbol t a)

-- | UniSymbol is instance of HasBody only if every sub-component has the same type
instance ( body ~ BodyOf (Symbol t Atom   a)
         , body ~ BodyOf (Symbol t Prefix a)
         , body ~ BodyOf (Symbol t Suffix a)
         , body ~ BodyOf (Symbol t Infix  a)
         , body ~ BodyOf (Symbol t Mixfix a)
         , HasBody (Symbol t Atom   a)
         , HasBody (Symbol t Prefix a)
         , HasBody (Symbol t Suffix a)
         , HasBody (Symbol t Infix  a)
         , HasBody (Symbol t Mixfix a)
         ) => HasBody (UniSymbol t a) where
    type BodyOf (UniSymbol t a) = BodyOf (Symbol t Atom a)
    body = lens getter (flip setter) where
        getter = \case
            Atom   t -> t ^. body
            Prefix t -> t ^. body
            Infix  t -> t ^. body
            Suffix t -> t ^. body
            Mixfix t -> t ^. body
        setter s = \case
            Atom   t -> Atom   $ t & body .~ s
            Prefix t -> Prefix $ t & body .~ s
            Infix  t -> Infix  $ t & body .~ s
            Suffix t -> Suffix $ t & body .~ s
            Mixfix t -> Mixfix $ t & body .~ s
