{-# LANGUAGE UndecidableInstances #-}

module Luna.Runtime.Dynamics where

import Prologue



-------------------
-- === Mode === ---
-------------------

-- === Definitions == --

--data Dynamics = Dynamics deriving (Show)

-- Dynamics types
data Dynamic = Dynamic deriving (Show)
data Static  = Static  deriving (Show)


-- === Accessors === --

type family Dynamics        a
type family WithDynamics ds a


-- === Utils === --

--type ToStatic  a = WithDynamics Static  a
--type ToDynamic a = WithDynamics Dynamic a

type family ByDynamics runtime static dynamic where
            ByDynamics Static  static dynamic = static
            ByDynamics Dynamic static dynamic = dynamic

type        SubSemiDynamics rt = rt ': SubDynamics rt

type family SubDynamics rt where
            SubDynamics Static  = '[]
            SubDynamics Dynamic = '[Static]


-- === Instances === --

-- Basic
type instance WithDynamics dyn Static  = dyn
type instance WithDynamics dyn Dynamic = dyn

-- Conversions
type instance To Static  a = WithDynamics Static  a
type instance To Dynamic a = WithDynamics Dynamic a
