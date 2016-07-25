{-# LANGUAGE UndecidableInstances #-}

module Luna.Runtime.Dynamics where

import Prologue



-------------------
-- === Mode === ---
-------------------

-- === Definitions == --

data Dynamics = Dynamics deriving (Show)

-- Dynamics types
data Dynamic = Dynamic deriving (Show)
data Static  = Static  deriving (Show)


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


-- === DEPRECIATED === --

type family Dynamics_OLD        a
type family WithDynamics_OLD ds a

-- Basic
type instance WithDynamics_OLD dyn Static  = dyn
type instance WithDynamics_OLD dyn Dynamic = dyn

-- Conversions
type instance To Static  a = WithDynamics_OLD Static  a
type instance To Dynamic a = WithDynamics_OLD Dynamic a
