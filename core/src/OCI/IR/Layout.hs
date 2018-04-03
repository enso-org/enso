{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout (module OCI.IR.Layout, module X) where
import Type.Data.Map as X ((:=))

import Prologue hiding (Default)

import qualified Type.Data.Map as Map

import Data.Tag
import Type.Data.Bool     (And)
import Type.Data.Map      (Map)
import Type.Data.Maybe
import Type.Data.Ord
import Type.Data.Property hiding (Set)



--------------------
-- === Layout === --
--------------------

-- Each IR component is part of a graph. In particular, these components
-- indicate the graph's nodes. Nodes have inputs and outputs and could have
-- one of many forms (can be defined with one of many constructors or even
-- one of many separate datatypes). A layout describes how the inputs and
-- outputs look like. No matter what the internal encoding is, what layers
-- a particular node uses, layout described what the connections are heading
-- to. A layout is just a type-level map of values. The keys are either
-- connection types (like 'Name' below is connection to node's name) or define
-- the internal node structure (like 'Model' below, denotes what data types
-- should we use to encode / decode the structure).

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


-- === Definition === --

type Layout lst = Layout__ (Map.FromAssocListRaw lst)
data Layout__ (map :: Map.Raw Type Type)


-- === API === --

type family Default key

type family Get (key :: Type) (layout :: Type) :: Type where
    Get key (Layout__ map) = FromMaybe (Default key) (Map.LookupRaw key map)
    Get key a              = Get key (ToLayout a)

type family Set key val layout where
    Set key val (Layout__ map) = Layout__ (Map.InsertRaw key val map)
    Set key val a              = Set key val (ToLayout a)

type family ToLayout a
type instance ToLayout (Layout__ m) = Layout__ m

type family Merge a b
type family MergeList lst where
    MergeList '[a]      = a
    MergeList (a ': as) = Merge a (MergeList as)


-- === Validation === --

type AssertEQ key layout val = Get key layout ~ val



----------------------
-- === Relayout === --
----------------------

-- The only way to properly implement the relayout functionality is by using
-- incoherent instances. No other Haskell tool (including closed type families)
-- would help us here. Let's consider the following layouts:
--
--     type L1 = Layout [ X := x , Y := y  ]
--     type L2 = Layout [ X := x , Y := y' ]
--
-- Such layouts could be a sub-layout of bigger layout, thus we need to be able
-- to check if they are identical even if not all types are resolved yet.
-- We cannot use closed type families here to check if both are the same and
-- if not progress further, because we want to progress further even if they
-- are different (for example we could relayout some sub-component to the most
-- general layout '()').
--
-- The solution is to check for the same type and succeed if it is the same
-- or check sub-components in other case. Checking for the same type have to
-- be implemented as incoherent instance not to block the further resolution.


-- === Definition === --

class Relayout       (src :: Type) (tgt :: Type)
class UnsafeRelayout (src :: Type) (tgt :: Type)


-- === API === --

relayout :: Relayout src tgt => src -> tgt
relayout = unsafeCoerce ; {-# INLINE relayout #-}

unsafeRelayout :: UnsafeRelayout src tgt => src -> tgt
unsafeRelayout = unsafeCoerce ; {-# INLINE unsafeRelayout #-}


-- === Instances === --

instance {-# INCOHERENT #-} Relayout a a

instance Relayout__ l l' => Relayout (Layout__ l) (Layout__ l')
type family Relayout__ src tgt :: Constraint where
    Relayout__ '[] '[] = ()
    Relayout__ ((k := v) ': l) ((k := v') ': l')
        = (SubRelayout__ v v', Relayout__ l l')
    Relayout__ _ _ = ()

class SubRelayout__ (src :: Type) (tgt :: Type)
instance {-# INCOHERENT   #-}                 SubRelayout__ a a
instance {-# OVERLAPPABLE #-} Relayout a b => SubRelayout__ a b
instance                                      SubRelayout__ a ()

