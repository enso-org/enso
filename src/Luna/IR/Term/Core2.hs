{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Core2 (module Luna.IR.Term.Core2, module X) where
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
import qualified OCI.IR.Layer.Internal               as Layer
import qualified OCI.IR.Layout                       as Layout

import Luna.IR.Component.Link              (type (*-*), Link)
import Luna.IR.Component.Term.Class        (Term, TermCons, Terms)
import Luna.IR.Component.Term.Construction (Creator)
import Luna.IR.Component.Term.Layer        (Model)
import Luna.IR.Component.Term.Layout
import Type.Data.Ord                       (Cmp)


import Luna.IR.Component.Term.Definition (Self)

----------------
-- === IR === --
----------------

type LinkTo t self a = Link (Layout.Get t a *-* Layout.Set Model self a)


-- === IR Atoms === ---

Term.define [d|

    data    Acc     a = Acc     { base  :: !(LinkTo Terms Self a)
                                , name  :: !(LinkTo Names Self a) }
    data    Missing a = Missing
    data    Unify   a = Unify   { left  :: !(LinkTo Terms Self a)
                                , right :: !(LinkTo Terms Self a) }
    newtype Var     a = Var     { name  :: Int }
 |]

