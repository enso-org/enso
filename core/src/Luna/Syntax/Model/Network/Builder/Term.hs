{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE PolyKinds                    #-}
{-# LANGUAGE NoMonomorphismRestriction    #-}

module Luna.Syntax.Model.Network.Builder.Term (module Luna.Syntax.Model.Network.Builder.Term, module X) where

import Luna.Syntax.Model.Network.Builder.Term.Class    as X
import Luna.Syntax.Model.Network.Builder.Term.Inferred as X (starAs, strAs, intAs, consAs, accAs, appAs, varAs, unifyAs, blankAs)
import Luna.Syntax.Model.Network.Builder.Term.Inferred as Inf


-- === Aliases for inferred term types === --

type InfTermBuilderAs t (term :: k) m a = Inf.TermBuilderAs t term m a
type InfTermBuilder     (term :: k) m a = Inf.TermBuilder     term m a


-- === Aliases for inferred term constructors === --

star'  = Inf.star
str'   = Inf.str
int'   = Inf.int
cons'  = Inf.cons
acc'   = Inf.acc
app'   = Inf.app
var'   = Inf.var
unify' = Inf.unify
sub'   = Inf.sub
blank' = Inf.blank
