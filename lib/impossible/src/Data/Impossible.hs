{-# LANGUAGE PolyKinds #-}

module Data.Impossible where

import Prelude
import GHC.Stack


-- === Definitions === --

data  Impossible = Impossible deriving (Show)
class ImpCls


-- === Utils === --

imp, impossible :: HasCallStack => a
impossibleTo    :: HasCallStack => String -> a
impossible   = withFrozenCallStack $ error "Impossible happened"          ; {-# INLINE impossible   #-}
impossibleTo = withFrozenCallStack . error . ("Impossible happened: " <>) ; {-# INLINE impossibleTo #-}
imp          = impossible                                                 ; {-# INLINE imp          #-}

-- === Primitives === --

-- type instance Imp = Impossible
-- type instance Imp = ImpCls
-- type instance Imp = '[Impossible]
-- type instance Imp = 'Just Imp
--
-- type instance Imp = 9223372036854775807     -- | ImpNat is defined as the maximum Int on 64 bit architecture. If GHC has some optimizations of Nat Values, it is nice not to exceed the value.
-- type instance Imp = "*I*M*P*O*S*S*I*B*L*E*" -- | Impossible definition for typelevel Symbols


-- === Parametrized impossibility === --

data ImpossibleM1 t1                         = ImpossibleM1 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM2 t1 t2                      = ImpossibleM2 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM3 t1 t2 t3                   = ImpossibleM3 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM4 t1 t2 t3 t4                = ImpossibleM4 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM5 t1 t2 t3 t4 t5             = ImpossibleM5 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM6 t1 t2 t3 t4 t5 t6          = ImpossibleM6 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM7 t1 t2 t3 t4 t5 t6 t7       = ImpossibleM7 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM8 t1 t2 t3 t4 t5 t6 t7 t8    = ImpossibleM8 deriving (Show, Functor, Traversable, Foldable)
data ImpossibleM9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = ImpossibleM9 deriving (Show, Functor, Traversable, Foldable)

-- type instance Imp = ImpossibleM1
-- type instance Imp = ImpossibleM2
-- type instance Imp = ImpossibleM3
-- type instance Imp = ImpossibleM4
-- type instance Imp = ImpossibleM5
-- type instance Imp = ImpossibleM6
-- type instance Imp = ImpossibleM7
-- type instance Imp = ImpossibleM8
-- type instance Imp = ImpossibleM9

-- Dummy instances
instance Monad       ImpossibleM1 where _ >>= _ = impossible ; {-# INLINE (>>=) #-}
instance Applicative ImpossibleM1 where pure  _ = impossible ; {-# INLINE pure  #-}
                                        _ <*> _ = impossible ; {-# INLINE (<*>) #-}


-- === Abbreviations === --

type Imp   = Impossible
type ImpM  = ImpossibleM1
type ImpM1 = ImpossibleM1
type ImpM2 = ImpossibleM2
type ImpM3 = ImpossibleM3
type ImpM4 = ImpossibleM4
type ImpM5 = ImpossibleM5
type ImpM6 = ImpossibleM6
type ImpM7 = ImpossibleM7
type ImpM8 = ImpossibleM8
type ImpM9 = ImpossibleM9

type ImpSymbol = "*I*M*P*O*S*S*I*B*L*E*"
