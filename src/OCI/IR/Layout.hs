{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout (module OCI.IR.Layout, module X) where

import Type.Data.Map as X ((:=))

import Prologue hiding (Default)
-- import Type.Data.Map
import Data.Tag
import Type.Data.Maybe
import Type.Data.Ord
import Type.Data.Property hiding (Set)
-- import Type.Data.Set.Proxy (Set)
import           Type.Data.Map (Map)
import qualified Type.Data.Map as Map



-- TermLayout_1 =
--      [ Model := ...
--      , Type  := ...
--      , Name  := ...
--      , Terms := ...
--      ]
--
-- LinkLayout_1 =
--      [ Source := ...
--      , Target := ...
--      ]


--------------------
-- === Layout === --
--------------------

type Layout lst = Layout__ (Map.FromAssocListRaw lst)
data Layout__ (map :: Map.Raw Type Type)

type family Default key

type family Get (key :: Type) (layout :: Type) :: Type where
    Get key (Layout__ map) = FromMaybe (Default key) (Map.LookupRaw key map)
    Get key _            = Default key

type family Set key val layout where
    Set key val (Layout__ map) = Layout__ (Map.InsertRaw key val map)




type family ToLayout a
type instance ToLayout (Layout__ m) = Layout__ m


-- === Validation === --

type AssertEQ key layout val = Get key layout ~ val


--
-- -- === Types === --
--
-- type BranchMap = Map Type Type
--
--
-- -- === Class === --
--
-- type family GetBase     layout
-- type family SetBase     base layout
-- type family GetBranches layout :: BranchMap
-- type family SetBranches (branches :: BranchMap) layout
--
--
-- -- === API === --
--
-- type SubLayout t layout = FromMaybe (Default t) (GetBranches layout !? t)
--
--
--
-- --------------------
-- -- === Nested === --
-- --------------------
--
-- data Nested base (branches :: BranchMap)
-- type (-<) = Nested
--
-- type instance GetBase       (a -< _) = a
-- type instance SetBase     a (_ -< b) = a -< b
-- type instance GetBranches   (_ -< b) = b
-- type instance SetBranches b (a -< _) = a -< b
