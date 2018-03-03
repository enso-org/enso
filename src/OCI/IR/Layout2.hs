{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeInType           #-}

module OCI.IR.Layout2 where

import Prologue
import qualified Prologue as P
-- import Type.Data.Map
import Type.Data.Property hiding (Set)
import Type.Data.Maybe
import Data.Tag
import Type.Data.Ord
-- import Type.Data.Set.Proxy (Set)
import Type.Data.Set (Set)
import Type.Data.Map (Map)
import qualified Type.Data.Map as Map
import qualified Type.Data.Set as Set


--
-- L1 = Layout $ 'Map
--   '[ Model := Draft -< Layout [...]
--    , Type  := Value -< Layout [...]
--    ]
--
-- B1 = Branch Model L1
-- >> Draft -< [...]
--
-- GetBranchBase B1
-- >> Draft
--
-- Children B1
-- >> [...]
--
--
--
-- --------------------------
-- -- === Associations === --
-- --------------------------
--
-- -- data (:=) (a :: Type) (b :: Type)
--
--
--
-- --------------------
-- -- === Layout === --
-- --------------------
--
-- -- === Layouts === --
--
-- data Layout (layout :: Map Type Type)
--
--
-- -- === API === --
--
-- type family GetBranch key layout
-- type family SetBranch key branch layout
-- type family DefBranch key
--
--
-- -- === Utils === --
--
-- type EmptyLayout = Layout Map.Empty
--
--
-- -- === Instances === --
--
-- type instance GetBranch t   (Layout s) = FromMaybe (DefBranch t) (s !? t)
-- type instance SetBranch t b (Layout s) = Layout (Map.Insert t b s)
--
--
-- ---------------------------
-- -- === Branch Layout === --
-- ---------------------------
--
-- -- === Definition === --
--
-- infixr 7 -<
-- type (-<) = BranchLayout
-- data BranchLayout (a :: Type) (b :: Type)
--
--
-- -- === API === --
--
-- type family GetBranchBase             layout
-- type family SetBranchBase     newBase layout
-- type family GetBranchChildren         layout
--
--
-- -- === Instances === --
--
-- type instance GetBranchBase       (a -< _) = a
-- type instance SetBranchBase     a (_ -< b) = a -< b
-- type instance GetBranchChildren   (_ -< b) = b
--
--
-- -- === Utils === --
--
-- type EmptyBranch a = a -< EmptyLayout
--
--
-- -------------------
-- -- === Utils === --
-- -------------------
--
-- type SubLayout key layout = GetBranchChildren (GetBranch key layout)
-- type Base      key layout = GetBranchBase     (GetBranch key layout)
-- type Rebase    key base layout = SetBranch key (SetBranchBase base (GetBranch key layout)) layout
--
--
--
-- --------------------
-- -- === Layout === --
-- --------------------
--
-- -- === Definition === --
--
--
--
--
--
--
--
-- -- === Instances === --








-- === Instances === --







--
-- --------------------
-- -- === Layout === --
-- --------------------
--
-- -- === Definition === --
--
-- data LAYOUT
-- type LayoutTag = Tag LAYOUT
--
--
-- -- === Modification === --
--
-- type family Base         layout
-- type family Rebase       base   layout
-- type family GetSublayout tp     layout
-- type family DefLayout    tp
--
--
--
-- ---------------------------
-- -- === Nested Layout === --
-- ---------------------------
--
-- -- === Definition === --
--
-- infixr 7 -<
-- type a -< b = Nested a b
-- data Nested (a :: P.Type) (b :: Set P.Type)
--
--

--
--
-- --------------------------
-- -- === Associations === --
-- --------------------------
--
-- data k := v
-- type instance Cmp (k := _) (k' := _) = Cmp k k'
--
--
--
-- --------------------------------
-- -- === Basic Layout Types === --
-- --------------------------------
--
-- -- === Definition === --
--
-- data TERM; type Term = LayoutTag TERM
-- data TYPE; type Type = LayoutTag TYPE
-- data NAME; type Name = LayoutTag NAME
--
-- defOrder [''Name, ''Type, ''Term]
--
-- -- === Utils === --
--
-- type family SetSublayout k v l where
--     SetSublayout k v (a -< b) = a -< (Set.Insert (k := v) b)
--     SetSublayout k v a        = a -< (Set.Singleton (k := v))
--
-- type l -*  t = SetSublayout Term t l
-- type l -:: t = SetSublayout Type t l
-- type l -#  t = SetSublayout Name t l
--
--
-- -------------------------
-- -- === Conversions === --
-- -------------------------
--
-- -- type family Coerce
