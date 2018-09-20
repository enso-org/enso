{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Transform.Desugar.DesugarFieldModifiers where

import Prologue

import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Map                         as Map
import qualified Data.Mutable.Class               as Mutable
import qualified Data.Vector.Storable.Foreign     as Vector
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.IR.Term.Ast.Invalid         as Invalid
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Basic                  as Pass
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Data.UniqueNameGen     as NameGen
import qualified Luna.Syntax.Text.Lexer.Symbol    as Syntax

import Data.Map            (Map)
import Luna.Pass.Data.Root (Root (Root))



---------------------------------------------
-- === DesugarFieldModifiers Pass === --
---------------------------------------------

data DesugarFieldModifiers

type instance Pass.Spec DesugarFieldModifiers t
   = DesugarFieldModifiersSpec t

type family DesugarFieldModifiersSpec t where
    DesugarFieldModifiersSpec (Pass.In  Pass.Attrs)
        = '[Root, NameGen.UniqueNameGen]
    DesugarFieldModifiersSpec (Pass.Out Pass.Attrs)
        = '[Root, NameGen.UniqueNameGen]
    DesugarFieldModifiersSpec t = Pass.BasicPassSpec t


-- | Desugars nested fields modifications in to simpler method calls.
--
-- Conventions: In this description I'm using <- to denote unifications
-- and = to denote field setters, to avoid notation confusion.
-- Moreover, unspaced =, e.g. `a.foo=` is just part of a method name,
-- not a special operator. This means `a.foo=` calls method `foo=` on a.
-- `a $= f` denotes modifying `a` by applying `f`.
--
-- First step of this desugaring is transforming modifications into
-- their most general form, i.e. `a.foo.bar.baz $= f`
-- This means that `a.foo.bar = b` becomes `a.foo.bar $= (x: b)`
-- and `a.foo.bar op= b` becomes `a.foo.bar $= (x: op x b)`.
--
-- Then, modifications are desugared according to the following
-- formal description:
-- Desugar(A $= f)          ==> f A
-- Desugar(A.f1.f2... $= f) ==> (x: t <- x; t.f1= Desugar((t.f1).f2... $= f)) A
--
-- It is worth noting why do we need the fresh lambda binder and
-- the `t <- x` part inside it.
-- This is due to Luna's lazy effects semantics. In particular,
-- if `A` executed some side effects, they would potentially be re–executed
-- multiple times, which is not expected when writing `A.foo.bar = 10`.
-- This allows us to execute these effects exactly once, saving the result
-- in a freshly created binding variable.
--
instance Pass.Definition TC.Stage DesugarFieldModifiers where
    definition = do
        Root root <- Attr.get
        root'     <- desugarModifiers root
        Attr.put $ Root root'

buildUpdateStack :: IR.SomeTerm -> [IR.Name] -> IR.SomeTerm
                 -> TC.Pass DesugarFieldModifiers IR.SomeTerm
buildUpdateStack tgt path mod = case path of
    []   -> do
        IR.app' mod tgt
    n:ns -> do
        binder   <- IR.var =<< NameGen.generateName
        cacheVar <- IR.var =<< NameGen.generateName
        cache    <- IR.unify cacheVar binder
        v        <- IR.var n
        nextTgt  <- IR.acc' cacheVar v
        nextRes  <- buildUpdateStack nextTgt ns mod
        vSetter  <- IR.var $ convert
                        $ Syntax.fieldSetterName $ convert n
        setter   <- IR.acc cacheVar vSetter
        modded   <- IR.app setter nextRes
        body     <- IR.seq cache modded
        lam      <- IR.lam binder body
        IR.app' lam tgt


desugarModifiers :: IR.SomeTerm -> TC.Pass DesugarFieldModifiers IR.SomeTerm
desugarModifiers e = Layer.read @IR.Model e >>= \case
    Uni.Update tgt' path' val' -> do
        tgt  <- desugarModifiers =<< IR.source tgt'
        val  <- desugarModifiers =<< IR.source val'
        path <- Mutable.toList path'

        modBinder <- IR.var =<< NameGen.generateName
        mod       <- IR.lam' modBinder val

        replacement <- buildUpdateStack tgt path mod
        IR.replace replacement e

        pure replacement
    Uni.Modify tgt' path' opName val' -> do
        tgt  <- desugarModifiers =<< IR.source tgt'
        val  <- desugarModifiers =<< IR.source val'
        path <- Mutable.toList path'

        binderName   <- NameGen.generateName
        modBinderIn  <- IR.var binderName
        modBinderOut <- IR.var binderName
        op           <- IR.var opName
        applied      <- IR.app op modBinderOut
        modBody      <- IR.app applied val
        mod          <- IR.lam' modBinderIn modBody

        replacement <- buildUpdateStack tgt path mod
        IR.replace replacement e

        pure replacement
    _ -> do
        inps <- IR.inputs e
        ComponentList.mapM_ (desugarModifiers <=< IR.source) inps
        pure e

