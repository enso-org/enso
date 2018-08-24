{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Map (module Type.Data.Map, module X) where

import Type.Data.Property as X

import Data.Kind
import Data.Proxy        (Proxy)
import Prelude
import Type.Data.Bool
import Type.Data.Maybe
import Type.Data.Ord
import Type.Data.Wrapped


--------------------------
-- === Map - PART 1 === --
--------------------------
-- --!!! Its defined in such way because of GHC BUG: https://ghc.haskell.org/trac/ghc/ticket/14668
-- TODO: Move it and refactor to Type.Data.Property when the bug gets resolved

type Raw (k :: Type) (v :: Type) = [Assoc k v]
newtype Map k v = Map (Raw k v)
type family FromMap m where FromMap ('Map m) = m

data Assoc a b = Assoc a b
type (:=) = 'Assoc
type AssocP = 'Assoc


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
type instance 'Map ((mk := mv) ': ms) --!? k = If (mk == k) ('Just mv) ('Map ms --!? k)
type instance 'Map ('[])              --!? k = 'Nothing

type instance KeyKind (Proxy (map :: Map k v)) = KeyKind map
type instance ValKind (Proxy (map :: Map k v)) = ValKind map
type instance Proxy (map :: Map mk mv) --!? k = map --!? k



----------------------------------------------
-- === MOCKUP UNTIL GHC BUG IS RESOLVED === --
----------------------------------------------

-- -- === Lookup === --
--
-- type instance 'Map '[] !? k = 'Nothing
-- type instance 'Map ('(mk,mv) ': ms) !? k =
--     If (mk == k) ('Just mv) (('Map ms) !? k)
--
--
-- -- === Set === --
--
-- type instance Set ('Map '[]) k v = 'Map '[ '(k,v) ]
-- type instance Set ('Map ('(mk, mv) ': ms)) k v = If (mk == k)
--     ('Map ('(mk, v) ': ms))
--     ('Map ('(mk,mv) ': FromMap (Set ('Map ms) k v)))
--
-- type instance Proxy (map :: Map mk mv) !? k = map !? k





type Empty = 'Map '[]

type family Insert k v m where Insert k v ('Map m) = 'Map (InsertRaw k v m)
type family InsertRaw (k :: kk) (v :: kv) (m :: [Assoc kk kv]) :: [Assoc kk kv] where
    InsertRaw k v '[]             = '[k := v]
    InsertRaw k v ((k := _) ': s) = (k := v) ': s
    InsertRaw k v ((l := w) ': s) = If (k < l)
         ((k := v) ': (l := w) ': s)
         ((l := w) ': InsertRaw k v s)


type family Lookup     k m where Lookup k ('Map m) = LookupRaw k m
type        LookupRaw' k m = FromJust (LookupRaw k m)
type family LookupRaw (k :: kk) (m :: [Assoc kk kv]) :: Maybe kv where
    LookupRaw k '[]             = 'Nothing
    LookupRaw k ((k := v) ': _) = 'Just v
    LookupRaw k (_        ': s) = LookupRaw k s


type family FromAssocListRaw (lst :: [Assoc k v]) :: [Assoc k v] where
    FromAssocListRaw '[]            = '[]
    FromAssocListRaw (k := v ': as) = InsertRaw k v (FromAssocListRaw as)


-- === Instances === --

type instance (m :: Map Type Type) !? k = Lookup k m

