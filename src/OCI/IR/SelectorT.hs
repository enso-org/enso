{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.SelectorT where

import Prologue

import qualified Type.Data.List as List


-- type (/) = Selector
-- data Selector base child

-----------------------
-- === Selectors === --
-----------------------

-- === Definition === --

-- data SingleSelector a = SingleSelector a
-- data ChoiceSelector a = ChoiceSelector [Selector a]
data Selector       a = Single a
                      | Choice [Selector a]

-- data Chain = Chain [Type]
-- type family FromChain a where FromChain (Chain ts) = ts
--

-- === SubSelector === --

-- | SubSelector has to have kind 'Type' because we need it to be placed
--   next to ordinary data in list smart selectors, for example:
--   >> a / [b, c/d]

data SubSelector (base :: Selector a) (child :: Selector a)
type base / child = SubSelector (ToSelector base) (ToSelector child)



-- === Conversions === --

type family ToSelector (a :: ak) :: Selector k where
    ToSelector (a :: Selector k)   = a
    ToSelector (a :: [Selector k]) = 'Choice a
    ToSelector (a :: [Type])       = ToSelector (List.Map 'Single a)
    ToSelector (a :: ak)           = 'Single a

--
-- -- === Expand === --
--
-- data TreeList__ base child
--
-- type family ExpandTreeList__ (lst :: Type) :: [Type] where
--     ExpandTreeList__ (TreeList__ base child) =
--         List.Append (ExpandTreeList__ base) (ExpandTreeList__ child)
--     ExpandTreeList__ a = '[a]
--
-- type family ExpandAfter__ (lsts :: [Type]) :: [[Type]] where
--     ExpandAfter__ '[]       = '[]
--     ExpandAfter__ (l ': ls) = ExpandTreeList__ l ': ExpandAfter__ ls
--
-- type family Expand__ (s :: k) :: [Type] where
--     Expand__ ('Single           s)      = Expand__ s
--     Expand__ ('Choice         s)      = Expand__ s
--     Expand__ ('SingleSelector   s)      = '[s]
--     Expand__ ('ChoiceSelector s)      = ExpandEvery__ s
--     Expand__ (SubSelector base child) = List.CartesianWith TreeList__
--                                         (Expand__ base) (Expand__ child)
--     Expand__ (a :: [k])               = ExpandEvery__ a
    -- Expand__ (a :: Type)              = '[a]
--
-- type family ExpandEvery__ (s :: [k]) :: [Type] where
--     ExpandEvery__ '[] = '[]
--     ExpandEvery__ (s ': ss) = List.Append (Expand__ s) (ExpandEvery__ ss)
--
-- type family Expand (s :: k) :: [[Type]] where
--     Expand s = ExpandAfter__ (Expand__ s)


--
-- a / [b, c/d]

-- a co gdyby zaimplementowac to mniej type safe?
