-- {-# LANGUAGE NoDuplicateRecordFields #-}
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
import qualified Luna.IR.Component.Term.Definition   as Term
import qualified Luna.IR.Component.Term.Discovery    as Discovery
import qualified Luna.IR.Component.Term.Layer        as Layer
import qualified Luna.IR.Term.Format                 as Format
import qualified OCI.Data.Name                       as Name
import qualified OCI.IR.Layer.Internal               as Layer
import qualified OCI.IR.Layout                       as Layout

import Luna.IR.Component.Link              (type (*-*), Link)
import Luna.IR.Component.Term.Class        (Term, TermCons, Terms)
import Luna.IR.Component.Term.Construction (Creator)
import Luna.IR.Component.Term.Layer        (Model)
import Luna.IR.Component.Term.Layout
import Type.Data.Ord                       (Cmp)


import Luna.IR.Component.Term.Definition (Self)

type LinkTo t self a = Link (Layout.Get t a *-* Layout.Set Model self a)


----------------
-- === IR === --
----------------

-- === IR Atoms === ---

Term.define ''Format.Thunk [d|
    data Acc     a = Acc     { base  :: LinkTo Terms Self a
                             , name  :: LinkTo Names Self a }
    data Unify   a = Unify   { left  :: LinkTo Terms Self a
                             , right :: LinkTo Terms Self a }
    |]

Term.define ''Format.Draft [d|
    data Var     a = Var     { name  :: Name.Ref }
    |]

Term.define ''Format.Phrase [d|
    data Missing a = Missing
    |]





-- === Smart constructors === --

var :: Creator Var m => Name.Ref -> m (Term Var)
var = Term.uncheckedNew . Var ; {-# INLINE var #-}

missing :: Creator Missing m => m (Term Missing)
missing = Term.uncheckedNew Missing ; {-# INLINE missing #-}

acc :: Creator Acc m => Term base -> Term name -> m (Term (Acc -* base -# name))
acc base name = Term.newM $ \self -> Acc <$> Link.new base self
                                         <*> Link.new name self
{-# INLINE acc #-}

unify :: Creator Unify m => Term left -> Term right
                         -> m (Term (Unify -* (Layout.Merge left right)))
unify left right = Term.newM
    $ \self -> Unify <$> fmap Layout.unsafeRelayout (Link.new left  self)
                     <*> fmap Layout.unsafeRelayout (Link.new right self)
{-# INLINE unify #-}





type instance Cmp Model Terms = 'LT
type instance Cmp Terms Model = 'GT


type instance Cmp Model Names = 'LT
type instance Cmp Names Model = 'GT

type instance Cmp Names Terms = 'LT
type instance Cmp Terms Names = 'GT

