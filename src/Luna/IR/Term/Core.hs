{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Core where

import Prologue

import qualified Data.Tag                            as Tag
import           Luna.IR.Component.Link              as Link
import qualified Luna.IR.Component.Link.TH           as Link
import qualified Luna.IR.Component.Term.Class        as Term
import qualified Luna.IR.Component.Term.Construction as Term
import qualified OCI.IR.Layer.Internal               as Layer
import qualified OCI.IR.Layout                       as Layout

import Foreign.Storable.Deriving     (deriveStorable)
import Foreign.Storable1.Deriving    (deriveStorable1)
import Luna.IR.Component.Term.Class  (Term, TermCons, Terms)
import Luna.IR.Component.Term.Layer (Model)
import Luna.IR.Component.Term.Layout
import Type.Data.Ord                 (Cmp)



----------------
-- === IR === --
----------------

-- === IR Atoms === ---


Tag.familyInstance "TermCons" "Var"
newtype ConsVar a = Var
    { __name :: Int
    } deriving (Show, Eq)
type instance Term.TagToCons Var     = ConsVar
type instance Term.ConsToTag ConsVar = Var
makeLenses      ''ConsVar
deriveStorable  ''ConsVar
deriveStorable1 ''ConsVar
Link.discover   ''ConsVar

Tag.familyInstance "TermCons" "Acc"
data ConsAcc a = Acc
    { __base :: !(Link (Layout.Get Terms a *-* Layout.Set Model Acc a))
    , __name :: !(Link (Layout.Get Names a *-* Layout.Set Model Acc a))
    } deriving (Show, Eq)
type instance Term.TagToCons Acc     = ConsAcc
type instance Term.ConsToTag ConsAcc = Acc
makeLenses      ''ConsAcc
deriveStorable  ''ConsAcc
deriveStorable1 ''ConsAcc
Link.discover   ''ConsAcc

Tag.familyInstance "TermCons" "Missing"
data ConsMissing a = Missing deriving (Show, Eq)
type instance Term.TagToCons Missing     = ConsMissing
type instance Term.ConsToTag ConsMissing = Missing
makeLenses      ''ConsMissing
deriveStorable  ''ConsMissing
deriveStorable1 ''ConsMissing
Link.discover   ''ConsMissing

data UniTerm a
    = UniTermVar     !(ConsVar     a)
    | UniTermAcc     !(ConsAcc     a)
    | UniTermMissing !(ConsMissing a)
    deriving (Show, Eq)
makeLenses      ''UniTerm
deriveStorable  ''UniTerm
deriveStorable1 ''UniTerm
Link.discover   ''UniTerm

type instance Term.Uni = UniTerm

instance Term.IsUni ConsVar     where toUni = UniTermVar     ; {-# INLINE toUni #-}
instance Term.IsUni ConsAcc     where toUni = UniTermAcc     ; {-# INLINE toUni #-}
instance Term.IsUni ConsMissing where toUni = UniTermMissing ; {-# INLINE toUni #-}




instance Layer.DataInitializer UniTerm where
    initData = UniTermMissing Missing ; {-# INLINE initData #-}



-- === Smart constructors === --

#define CTX(name) (Term.Creator name m, Link.Creator m)

var :: CTX(Var) => Int -> m (Term Var)
var = Term.uncheckedNew . Var ; {-# INLINE var #-}

missing :: CTX(Missing) => m (Term Missing)
missing = Term.uncheckedNew Missing ; {-# INLINE missing #-}

acc :: CTX(Acc) => Term base -> Term name -> m (Term (Acc -* base -# name))
acc base name = Term.newM $ \term -> Acc <$> Link.new base term <*> Link.new name term ; {-# INLINE acc #-}

#undef CTX






type instance Cmp Model Terms = 'LT
type instance Cmp Terms Model = 'GT


type instance Cmp Model Names = 'LT
type instance Cmp Names Model = 'GT

type instance Cmp Names Terms = 'LT
type instance Cmp Terms Names = 'GT

