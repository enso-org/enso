{-# LANGUAGE CPP                  #-}
{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Symbol (module Language.Symbol, module X) where
import Language.Symbol.Label as X

import Prologue hiding (Data, Symbol)




---------------------
-- === Symbols === --
---------------------

-- === Data === --

newtype Symbol t s a = Symbol (Data t s a)
type family Data t s a
makeLenses ''Symbol


-- === Symbol types === --

data Atom
data Prefix
data Suffix
data Infix
data Mixfix


-- === Utils === --

type FromSymbol t s a p = Convertible (Symbol t s a) p

symbol :: forall t s a p. FromSymbol t s a p => Data t s a -> p
symbol = convert . Symbol @t @s @a

atom   :: forall t a p. FromSymbol t Atom   a p => Data t Atom   a -> p
prefix :: forall t a p. FromSymbol t Prefix a p => Data t Prefix a -> p
suffix :: forall t a p. FromSymbol t Suffix a p => Data t Suffix a -> p
infixx :: forall t a p. FromSymbol t Infix  a p => Data t Infix  a -> p
mixfix :: forall t a p. FromSymbol t Mixfix a p => Data t Mixfix a -> p
atom   = symbol @t @Atom   @a
prefix = symbol @t @Prefix @a
suffix = symbol @t @Suffix @a
infixx = symbol @t @Infix  @a
mixfix = symbol @t @Mixfix @a


-- === Instances === --

deriving instance Show (Data t s a) => Show (Symbol t s a)

instance (s ~ s', t ~ t', a ~ a')
      => Convertible (Symbol t s a) (Symbol t' s' a') where convert = id


-- === Basic symbol types === --

data Expr
type ExprSymbol = Symbol Expr
type instance Data Expr Atom   a = a
type instance Data Expr Prefix a = a -> a
type instance Data Expr Suffix a = a -> a
type instance Data Expr Infix  a = a -> a -> a

data Phantom
type PhantomSymbol = Symbol Phantom
type instance Data Phantom Atom   a = a
type instance Data Phantom Prefix a = a
type instance Data Phantom Suffix a = a
type instance Data Phantom Infix  a = a
type instance Data Phantom Mixfix a = a



------------------------
-- === Properties === --
------------------------

-- === Body === --

class HasBody a where
    type family BodyOf a
    body :: Lens' a (BodyOf a)

instance HasBody (Symbol t s a) where
    type BodyOf  (Symbol t s a) = Data t s a
    body = wrapped

instance HasBody a => HasBody (Labeled l a) where
    type BodyOf (Labeled l a) = BodyOf a
    body = labeledContent . body



------------------------
-- === SomeSymbol === --
------------------------

-- === Definitions === --

data SomeSymbol t a
    = Atom   !(Symbol t Atom   a)
    | Prefix !(Symbol t Prefix a)
    | Suffix !(Symbol t Suffix a)
    | Infix  !(Symbol t Infix  a)
    | Mixfix !(Symbol t Mixfix a)


-- === Constructors === --

#define Conv (t ~ t', a ~ a') => Convertible
instance Conv (Symbol t Atom   a) (SomeSymbol t' a') where convert = Atom
instance Conv (Symbol t Prefix a) (SomeSymbol t' a') where convert = Prefix
instance Conv (Symbol t Suffix a) (SomeSymbol t' a') where convert = Suffix
instance Conv (Symbol t Infix  a) (SomeSymbol t' a') where convert = Infix
instance Conv (Symbol t Mixfix a) (SomeSymbol t' a') where convert = Mixfix
#undef Conv

-- === Instances === --

deriving instance
    ( Show (Data t Atom   a)
    , Show (Data t Prefix a)
    , Show (Data t Suffix a)
    , Show (Data t Infix  a)
    , Show (Data t Mixfix a)
    ) => Show (SomeSymbol t a)

-- | SomeSymbol is instance of HasBody only if every sub-component
--   has the same type.
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
         ) => HasBody (SomeSymbol t a) where
    type BodyOf (SomeSymbol t a) = BodyOf (Symbol t Atom a)
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

