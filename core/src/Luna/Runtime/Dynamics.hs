module Luna.Type.Dynamics where

import Prologue



-------------------
-- === Mode === ---
-------------------

data Dynamic = Dynamic deriving (Show)
data Static  = Static  deriving (Show)

-- Model conversion
type family ToStatic  a :: *
type family ToDynamic a :: *

type family Dynamics  a :: *


-- === Utils === --

type family ByRuntime runtime static dynamic where
    ByRuntime Static   static dynamic = static
    ByRuntime Dynamic  static dynamic = dynamic

type        SubSemiRuntimes rt = rt ': SubRuntimes rt
type family SubRuntimes     rt where SubRuntimes Static  = '[]
                                     SubRuntimes Dynamic = '[Static]


-- === Instances === --

type instance ToStatic Static  = Static
type instance ToStatic Dynamic = Static

type instance ToDynamic Static  = Dynamic
type instance ToDynamic Dynamic = Dynamic
