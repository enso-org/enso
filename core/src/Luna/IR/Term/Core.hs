{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Core where

import Prologue

import qualified Luna.IR.Component.Link              as Link
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.IR.Component.Term.Definition   as Term
import qualified Luna.IR.Component.Term.Layer        as Layer
import qualified Luna.IR.Term.Format                 as Format
import qualified OCI.IR.Layer                        as Layer
import qualified OCI.IR.Layout                       as Layout

import Luna.IR.Component.Term.Class      (Term, Terms)
import Luna.IR.Component.Term.Definition (LinkTo)
import Luna.IR.Component.Term.Layer      (Model)
import Luna.IR.Component.Term.Layout     (Names)
import OCI.Data.Name                     (Name)
import Type.Data.Ord                     (Cmp)


import Data.PtrList.Mutable (UnmanagedPtrList)

type List = UnmanagedPtrList



----------------
-- === IR === --
----------------

-- | Core IR terms definition. For more information on what the actual data
--   is created please refer to the documentation of the TH functions.

-- === IR Atoms === ---

Term.defineNoSmartCons [d|
 data Value = Top
 |]

Term.define [d|

 data Value
    = Cons      { name :: Name        , args  :: List (LinkTo Terms) }
    | App       { base :: LinkTo Terms, arg   :: LinkTo Terms        } -- FIXME: flip args

 data Thunk
    = Acc       { base :: LinkTo Terms, name  :: LinkTo Names }
    | Lam       { arg  :: LinkTo Terms, body  :: LinkTo Terms }

 data Phrase
    = Blank
    | Missing
    | Unify     { left :: LinkTo Terms, right :: LinkTo Terms }

 data Draft
    = Var { name :: Name }

 |]




-- === Smart constructors === --

-- | The smart constructor of 'Top' is special one, because its type link loops
--   to itself. All other smart constructors use 'top' as their initial type
--   representation.
top :: Term.Creator Top m => m (Term Top)
top = Term.uncheckedUntypedNewM $ \self -> do
    typeLink <- Link.new self self
    Layer.write @Layer.Type self (Layout.relayout typeLink)
    pure Top
{-# INLINE top #-}

instance Term.Creator Top m => Term.DefaultType m where
    defaultType = coerce <$> top ; {-# INLINE defaultType #-}



-------------------------
-- === TO AUTOMATE === --
-------------------------

type instance Cmp Model Terms = 'LT
type instance Cmp Terms Model = 'GT

type instance Cmp Model Names = 'LT
type instance Cmp Names Model = 'GT

type instance Cmp Names Terms = 'LT
type instance Cmp Terms Names = 'GT

