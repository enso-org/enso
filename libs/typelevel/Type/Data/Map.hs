{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Map (module Type.Data.Map, module X) where

import Type.Data.Property as X

import Prelude
import Data.Kind
import Type.Data.Bool
import Data.Proxy (Proxy)
import Type.Data.Maybe (FromJust)
import Type.Data.Wrapped


--------------------------
-- === Map - PART 1 === --
--------------------------
-- --!!! Its defined in such way because of GHC BUG: https://ghc.haskell.org/trac/ghc/ticket/14668
-- TODO: Move it and refactor to Type.Data.Property when the bug gets resolved

data Map (k :: Type) (v :: Type) = Map [(k,v)]
type family FromMap m where FromMap ('Map m) = m


----------------------
-- === Property === --
----------------------

type family KeyKind (obj :: a) :: t
type family ValKind (obj :: a) :: t


-- | Query 'obj' of any kind by 'key' to get 'val'. The dependencies
--   'obj' -> 'key kind' and 'obj' -> 'val kind' are provided by
--   'KeyKind' and 'ValKind' respectively. See Type.Data.Map as usage reference.
type family (obj :: a) --!? (key :: KeyKind obj) :: Maybe (ValKind obj)


-- | The '--!!' query operator is exactly like '--!?' but returns the result
--   directly instead of encoding it in Maybe. It raises compilation error
--   if the key is missing.
type family (obj :: a) --!! (key :: KeyKind obj) :: ValKind obj where
    obj --!! key = FromJust (obj --!? key)



--------------------------
-- === Map - PART 2 === --
--------------------------


type instance KeyKind (map :: Map k v) = k
type instance ValKind (map :: Map k v) = v
type instance 'Map ('(mk,mv) ': ms) --!? k = If (mk == k) ('Just mv) (('Map ms) --!? k)
type instance 'Map ('[])            --!? k = 'Nothing

type instance KeyKind (Proxy (map :: Map k v)) = KeyKind map
type instance ValKind (Proxy (map :: Map k v)) = ValKind map
type instance Proxy (map :: Map mk mv) --!? k = map --!? k



----------------------------------------------
-- === MOCKUP UNTIL GHC BUG IS RESOLVED === --
----------------------------------------------

-- === Lookup === --

type instance 'Map '[] !? k = 'Nothing
type instance 'Map ('(mk,mv) ': ms) !? k =
    If (mk == k) ('Just mv) (('Map ms) !? k)


-- === Set === --

type instance Set ('Map '[]) k v = 'Map '[ '(k,v) ]
type instance Set ('Map ('(mk, mv) ': ms)) k v = If (mk == k)
    ('Map ('(mk, v) ': ms))
    ('Map ('(mk,mv) ': FromMap (Set ('Map ms) k v)))

type instance Proxy (map :: Map mk mv) !? k = map !? k
