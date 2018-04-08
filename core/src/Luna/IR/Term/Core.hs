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

Term.defineNoSmartCons ''Format.Value [d|

    data Top a = Top

    |]

Term.define ''Format.Value [d|

    data Cons a = Cons
        { name :: Name
        , args :: List (LinkTo Terms a) }

    data App a = App
        { base :: LinkTo Terms a
        , arg  :: LinkTo Terms a } -- FIXME: flip args

    |]

Term.define ''Format.Thunk [d|

    data Acc a = Acc
        { base :: LinkTo Terms a
        , name :: LinkTo Names a
        }

    |]

Term.define ''Format.Phrase [d|

    data Blank   a = Blank

    data Missing a = Missing

    data Unify   a = Unify
        { left  :: LinkTo Terms a
        , right :: LinkTo Terms a }

    |]

Term.define ''Format.Draft [d|

    data Var a = Var
        { name :: Name }

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

