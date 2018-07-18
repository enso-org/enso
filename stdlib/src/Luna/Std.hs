{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Luna.Std where

import Prologue

import qualified Data.Map                     as Map
import qualified Luna.IR                      as IR
import qualified Luna.Runtime                 as Luna
import qualified Luna.Pass.Sourcing.Data.Def  as Def
import qualified Luna.Pass.Sourcing.Data.Unit as Unit
import qualified Luna.Prim.Base               as Base
import qualified Luna.Prim.Foreign            as Foreign
import qualified Luna.Prim.Time               as Time
import qualified Luna.Prim.WebSockets         as WebSockets
import qualified Luna.Prim.System             as System
import qualified Luna.Prim.HTTP               as HTTP
import qualified Luna.Std.Builder             as Builder

import Data.Map            (Map)
import Luna.Std.Finalizers (finalize, initFinalizersCtx)

stdlibImports :: [IR.Qualified]
stdlibImports = [ "Std.Base"
                , "Std.HTTP"
                , "Std.System"
                , "Std.Time"
                , "Std.WebSockets"
                , "Std.Foreign"
                , "Std.Foreign.C.Value"
                ]

stdlib :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => m (IO (), Unit.UnitRef)
stdlib = do
    finalizersCtx <- liftIO initFinalizersCtx

    baseFuncs    <- Base.exports       @graph finalizersCtx
    timeFuncs    <- Time.exports       @graph
    wsFuncs      <- WebSockets.exports @graph finalizersCtx
    sysFuncs     <- System.exports     @graph
    httpFuncs    <- HTTP.exports       @graph
    foreignFuncs <- Foreign.exports    @graph finalizersCtx

    let defs = Map.unions [baseFuncs, timeFuncs, wsFuncs, sysFuncs, httpFuncs, foreignFuncs]
        docDefs = Def.Documented def <$> defs
        unit = Unit.Unit (Def.DefsMap docDefs) def
        unitRef = Unit.UnitRef (Unit.Precompiled unit) (Unit.Imports stdlibImports)

    return (finalize finalizersCtx, unitRef)
