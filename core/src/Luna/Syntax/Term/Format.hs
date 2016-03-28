module Luna.Syntax.Term.Format where

import Prelude.Luna


------------------------------
-- === Evaluation Model === --
------------------------------

-- === Definitions === --

data Lit    = Lit    deriving (Show)
data Val    = Val    deriving (Show)
data Thunk  = Thunk  deriving (Show)
data Phrase = Phrase deriving (Show)
data Draft  = Draft  deriving (Show)

type Formats = '[Lit, Val, Thunk, Phrase, Draft]


-- TODO[WD]: refactor rest of the evaluation model here

-- REFACTOR IDEAS:

-- - Refaktor Network/Term oraz Network/Builder/Term/... poza Network, poniewaz sa to elementy ktore moga przydac sie nie tylko w Networku (moze tez w breadcrumbach?)
-- - Refaktor Graph/Class (ELEMENT, CONNECTION) - jako element i polaczenie w abstrakcyjnej strukturze (graf / plaska / breadcrumb etc)
--
--
-- po tym w netwroku zrobic aliasy na te konstukotry doajac do nich "node"
--
-- PROPOZYCJA:
--
-- Luna/Syntax/Network/Term ~> Luna/Evaluation/Model/Layered
-- Luna/Syntax/Network/Builder/Term -> Luna/Syntax/AST/Term/Builder
