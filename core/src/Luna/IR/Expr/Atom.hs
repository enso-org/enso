{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.IR.Expr.Atom where

import Luna.Prelude hiding (String, Integer, Rational, Curry)
import Data.Base
import Data.Phantom
import Data.Property
import Data.Reprx
import Type.Container (Every)
import Data.Families  (makeLunaComponents)


-- === Definition pragmas === --

makeLunaComponents "Atom" "Atomic"
    [ "Integer"
    , "Rational"
    , "String"
    , "Acc"
    , "App"
    , "Blank"
    , "Cons"
    , "Grouped"
    , "Lam"
    , "Missing"
    , "Star"
    , "Unify"
    , "Var"
    ]

type family AtomOf a ::  *
type family Atoms  a :: [*]


-- === AtomRep === --

newtype AtomRep = AtomRep TypeRep deriving (Eq)
makeWrapped ''AtomRep

atomRep' :: forall a. Typeable (AtomOf a) => AtomRep
atomRep' = wrap' $ typeRep (Proxy :: Proxy (AtomOf a)) ; {-# INLINE atomRep' #-}

atomRep :: forall a. Typeable (AtomOf a) => a -> AtomRep
atomRep _ = atomRep' @a ; {-# INLINE atomRep #-}


-- === Instances === --

type instance AtomOf      (Atomic a) = Atomic a
type instance Atoms       (Atomic a) = '[Atomic a]
type instance Access Atom (Atomic a) = Atomic a
type instance TypeRepr    (Atomic a) = TypeRepr a
