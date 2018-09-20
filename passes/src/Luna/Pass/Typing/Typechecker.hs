{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.Typechecker where

import Prologue

import qualified Luna.IR                               as IR
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Data.Root                   as Root
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Data.UniqueNameGen          as UniqueNameGen
import qualified Luna.Pass.Typing.Data.AccQueue        as AccQueue
import qualified Luna.Pass.Typing.Data.AppQueue        as AppQueue
import qualified Luna.Pass.Typing.Data.UniQueue        as UniQueue
import qualified Luna.Pass.Typing.Data.Progress        as Progress
import qualified Luna.Pass.Typing.Data.Target          as Target
import qualified Luna.Pass.Typing.Data.Typed           as Typed

import Luna.Pass.Typing.DefImporter      (DefImporter)
import Luna.Pass.Typing.MethodImporter   (MethodImporter)
import Luna.Pass.Typing.ConsImporter     (ConsImporter)
import Luna.Pass.Typing.Structural       (StructuralTyping)
import Luna.Pass.Typing.UniSolver        (UniSolver)
import Luna.Pass.Typing.AppSolver        (AppSolver)
import Luna.Pass.Typing.HeaderBuilder    (HeaderBuilder)
import Luna.Pass.Typing.ErrorPropagation (ErrorPropagation)

runTypechecker :: Target.Target -> IR.SomeTerm
               -> Typed.Units -> TC.Monad Typed.DefHeader
runTypechecker tgt root units = do
    Scheduler.registerAttr @Typed.Units
    Scheduler.registerAttr @Typed.DefHeader
    Scheduler.registerAttr @Progress.Progress
    Scheduler.registerAttr @Root.Root
    Scheduler.registerAttr @Target.Target
    Scheduler.registerAttr @UniqueNameGen.UniqueNameGen
    Scheduler.registerAttr @AppQueue.AppQueue
    Scheduler.registerAttr @UniQueue.UniQueue
    Scheduler.registerAttr @AccQueue.AccQueue

    Scheduler.setAttr $ Root.Root root
    Scheduler.setAttr units
    Scheduler.setAttr tgt
    Scheduler.enableAttrByType @Typed.DefHeader
    Scheduler.enableAttrByType @Progress.Progress
    Scheduler.enableAttrByType @UniqueNameGen.UniqueNameGen
    Scheduler.enableAttrByType @AppQueue.AppQueue
    Scheduler.enableAttrByType @UniQueue.UniQueue
    Scheduler.enableAttrByType @AccQueue.AccQueue

    Scheduler.registerPass @TC.Stage @DefImporter
    Scheduler.registerPass @TC.Stage @StructuralTyping
    Scheduler.registerPass @TC.Stage @UniSolver
    Scheduler.registerPass @TC.Stage @AppSolver
    Scheduler.registerPass @TC.Stage @HeaderBuilder
    Scheduler.registerPass @TC.Stage @ConsImporter
    Scheduler.registerPass @TC.Stage @MethodImporter
    Scheduler.registerPass @TC.Stage @ErrorPropagation

    Scheduler.runPassByType @DefImporter
    Scheduler.runPassByType @StructuralTyping
    Scheduler.runPassByType @ConsImporter

    loopWhileProgress

    Scheduler.runPassByType @ErrorPropagation
    Scheduler.runPassByType @HeaderBuilder

    Scheduler.getAttr @Typed.DefHeader

loopWhileProgress :: TC.Monad ()
loopWhileProgress = do
    Scheduler.runPassByType @AppSolver
    Progress.Progress appp <- Scheduler.getAttr
    Scheduler.runPassByType @MethodImporter
    Progress.Progress accp <- Scheduler.getAttr
    Scheduler.runPassByType @UniSolver
    Progress.Progress unip <- Scheduler.getAttr
    when (unip || appp || accp) loopWhileProgress

