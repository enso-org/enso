{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Core (module Luna.IR.Term.Core, module X) where
import Luna.IR.Component.Term.Construction as X (ConsTop (Top), Top, top)

import Prologue

import qualified Data.Tag                            as Tag
import qualified Foreign.Storable.Deriving           as Storable
import qualified Foreign.Storable1.Deriving          as Storable1
import qualified Luna.IR.Component.Link              as Link
import qualified Luna.IR.Component.Link.TH           as Link
import qualified Luna.IR.Component.Term.Class        as Term
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.IR.Component.Term.Layer        as Layer
import qualified OCI.IR.Layer.Internal               as Layer
import qualified OCI.IR.Layout                       as Layout

import Luna.IR.Component.Link        (type (*-*), Link)
import Luna.IR.Component.Term.Class  (Term, TermCons, Terms)
import Luna.IR.Component.Term.Layer  (Model)
import Luna.IR.Component.Term.Layout
import Type.Data.Ord                 (Cmp)

import qualified Data.Set.Mutable.Class as Set

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
makeLenses       ''ConsVar
Storable.derive  ''ConsVar
Storable1.derive ''ConsVar
Link.discover    ''ConsVar

Tag.familyInstance "TermCons" "Acc"
data ConsAcc a = Acc
    { __base :: !(Link (Layout.Get Terms a *-* Layout.Set Model Acc a))
    , __name :: !(Link (Layout.Get Names a *-* Layout.Set Model Acc a))
    } deriving (Show, Eq)
type instance Term.TagToCons Acc     = ConsAcc
type instance Term.ConsToTag ConsAcc = Acc
makeLenses       ''ConsAcc
Storable.derive  ''ConsAcc
Storable1.derive ''ConsAcc
Link.discover    ''ConsAcc

Tag.familyInstance "TermCons" "Unify"
data ConsUnify a = Unify
    { __left  :: !(Link (Layout.Get Terms a *-* Layout.Set Model Unify a))
    , __right :: !(Link (Layout.Get Terms a *-* Layout.Set Model Unify a))
    } deriving (Show, Eq)
type instance Term.TagToCons Unify     = ConsUnify
type instance Term.ConsToTag ConsUnify = Unify
makeLenses       ''ConsUnify
Storable.derive  ''ConsUnify
Storable1.derive ''ConsUnify
Link.discover    ''ConsUnify

Tag.familyInstance "TermCons" "Missing"
data ConsMissing a = Missing deriving (Show, Eq)
type instance Term.TagToCons Missing     = ConsMissing
type instance Term.ConsToTag ConsMissing = Missing
makeLenses       ''ConsMissing
Storable.derive  ''ConsMissing
Storable1.derive ''ConsMissing
Link.discover    ''ConsMissing

data UniTerm a
    = UniTermTop     !(ConsTop     a)
    | UniTermVar     !(ConsVar     a)
    | UniTermAcc     !(ConsAcc     a)
    | UniTermUnify   !(ConsUnify   a)
    | UniTermMissing !(ConsMissing a)
    deriving (Show, Eq)
makeLenses       ''UniTerm
Storable.derive  ''UniTerm
Storable1.derive ''UniTerm
Link.discover    ''UniTerm

type instance Term.Uni = UniTerm

instance Term.IsUni ConsTop     where toUni = UniTermTop     ; {-# INLINE toUni #-}
instance Term.IsUni ConsVar     where toUni = UniTermVar     ; {-# INLINE toUni #-}
instance Term.IsUni ConsAcc     where toUni = UniTermAcc     ; {-# INLINE toUni #-}
instance Term.IsUni ConsUnify   where toUni = UniTermUnify   ; {-# INLINE toUni #-}
instance Term.IsUni ConsMissing where toUni = UniTermMissing ; {-# INLINE toUni #-}




instance Layer.DataInitializer UniTerm where
    initStaticData = Just $ UniTermMissing Missing ; {-# INLINE initStaticData #-}



-- === Constructor helpers === --

type Creator tag m =
    ( Term.UntypedCreator tag m
    , Term.UntypedCreator Top m
    , Link.Creator m
    , Layer.Writer Terms Layer.Type m
    )

uncheckedNewM :: Creator tag m
              => (Term any -> m (Term.TagToCons tag layout)) -> m (Term any)
uncheckedNewM !cons = Term.uncheckedUntypedNewM $ \self -> do
    typeTerm <- top
    typeLink <- Link.new typeTerm self
    Layer.write @Layer.Type self (Layout.unsafeRelayout typeLink)
    cons self
{-# INLINE uncheckedNewM #-}

newM :: (Creator tag m, Term.LayoutInference tag layout)
     => (Term layout -> m (Term.TagToCons tag layout)) -> m (Term layout)
newM = uncheckedNewM ; {-# INLINE newM #-}

uncheckedNew :: Creator tag m => Term.TagToCons tag layout -> m (Term any)
uncheckedNew = uncheckedNewM . const . pure ; {-# INLINE uncheckedNew #-}


-- === Smart constructors === --

var :: Creator Var m => Int -> m (Term Var)
var = uncheckedNew . Var ; {-# INLINE var #-}

missing :: Creator Missing m => m (Term Missing)
missing = uncheckedNew Missing ; {-# INLINE missing #-}

acc :: Creator Acc m => Term base -> Term name -> m (Term (Acc -* base -# name))
acc base name = newM $ \self -> Acc <$> Link.new base self
                                    <*> Link.new name self
{-# INLINE acc #-}

-- FIXME: double left vvv
unify :: Creator Unify m => Term left -> Term left -> m (Term (Unify -* left))
unify left right = newM $ \self -> Unify <$> Link.new left  self
                                         <*> Link.new right self
{-# INLINE unify #-}





type instance Cmp Model Terms = 'LT
type instance Cmp Terms Model = 'GT


type instance Cmp Model Names = 'LT
type instance Cmp Names Model = 'GT

type instance Cmp Names Terms = 'LT
type instance Cmp Terms Names = 'GT

