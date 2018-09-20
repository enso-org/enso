{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Luna.IR.Term.Core where

import Prologue

import qualified Data.Graph.Component.Edge              as Link
import qualified Data.Graph.Component.Node.Construction as Term
import qualified Data.Graph.Component.Node.Layer        as Layer
import qualified Data.Graph.Data.Layer.Class            as Layer
import qualified Data.Graph.Data.Layer.Layout           as Layout
import qualified Luna.IR.Term.Format                    as Format
import qualified OCI.IR.Term.Definition                 as Term

import OCI.Data.Name          (Name, Qualified)
import OCI.IR.Term.Class      (Term, Terms)
import OCI.IR.Term.Definition (LinkTo, LinksTo, List)
import OCI.IR.Term.Layout     ()



----------------
-- === IR === --
----------------

-- | Core IR terms definition. For more information on what the actual data
--   is created please refer to the documentation of the 'Term.define' function.

-- === IR Atoms === ---

Term.define [d|

 data Value
    = App          { base :: LinkTo Terms , arg  :: LinkTo Terms  }
    | ResolvedCons { unit :: Qualified    , cls  :: Name
                   , name :: Name         , args :: LinksTo Terms }
    | Top_

 data Thunk
    = Acc     { base :: LinkTo Terms, name  :: LinkTo Terms                    }
    | Lam     { arg  :: LinkTo Terms, body  :: LinkTo Terms                    }
    | Match   { arg  :: LinkTo Terms, ways  :: LinksTo Terms                   }
    | Update  { base :: LinkTo Terms, path  :: List Name, val :: LinkTo Terms  }

 data Phrase
    = Blank
    | Missing
    | Unify   { left :: LinkTo Terms, right :: LinkTo Terms                    }

 data Draft
    = Var         { name :: Name                     }
    | ResolvedDef { unit :: Qualified , name :: Name }

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

