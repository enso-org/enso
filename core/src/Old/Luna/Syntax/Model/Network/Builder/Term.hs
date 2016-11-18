{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE PolyKinds                    #-}
{-# LANGUAGE NoMonomorphismRestriction    #-}

module Old.Luna.Syntax.Model.Network.Builder.Term (module Old.Luna.Syntax.Model.Network.Builder.Term, module X) where

import Old.Luna.Syntax.Model.Network.Builder.Term.Class    as X
import Old.Luna.Syntax.Model.Network.Builder.Term.Inferred as X (starAs, strAs, intAs, consAs, accAs, appAs, varAs, unifyAs, blankAs)
import Old.Luna.Syntax.Model.Network.Builder.Term.Inferred as Inf


-- === Aliases for inferred term types === --

type InfTermBuilderAs t (term :: k) m a = Inf.TermBuilderAs_OLD t term m a
type InfTermBuilder     (term :: k) m a = Inf.TermBuilder_OLD     term m a


-- === Aliases for inferred term constructors === --

star'   = Inf.star
str'    = Inf.str
int'    = Inf.int
double' = Inf.double
cons'   = Inf.cons
acc'    = Inf.acc
app'    = Inf.app
var'    = Inf.var
unify'  = Inf.unify
match'  = Inf.match
blank'  = Inf.blank
