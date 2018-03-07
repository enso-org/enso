{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeInType           #-}

module OCI.IR.Layout3 where

import Prologue
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



-- L1 = Draft -< '[ Type    := ...
--                , Name    := ...
--                , SubTerm := ...
--                ]


--------------------
-- === Layout === --
--------------------

-- === Types === --

type BranchMap = Map Type Type


-- === Class === --

type family GetBase     layout
type family SetBase     base layout
type family GetBranches layout :: BranchMap
type family SetBranches (branches :: BranchMap) layout
type family DefLayout   key


-- === API === --

type SubLayout t layout = FromMaybe (DefLayout t) (GetBranches layout !? t)



--------------------
-- === Nested === --
--------------------

data Nested base (branches :: BranchMap)
type (-<) = Nested

type instance GetBase       (a -< _) = a
type instance SetBase     a (_ -< b) = a -< b
type instance GetBranches   (_ -< b) = b
type instance SetBranches b (a -< _) = a -< b
