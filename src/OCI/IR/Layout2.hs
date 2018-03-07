{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeInType           #-}

module OCI.IR.Layout2 where

import Prologue
-- import Type.Data.Map
import Type.Data.Property hiding (Set)
import Type.Data.Maybe
import Data.Tag
import Type.Data.Ord
-- import Type.Data.Set.Proxy (Set)
import Type.Data.Map (Map)
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

data Layout (map :: Map.Raw Type Type)

type family DefLayout key

type family Get key layout where
    Get key (Layout map) = FromMaybe (DefLayout key) (Map.LookupRaw key map)

type family Set key val layout where
    Set key val (Layout map) = Layout (Map.InsertRaw key val map)


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
-- type SubLayout t layout = FromMaybe (DefLayout t) (GetBranches layout !? t)
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
