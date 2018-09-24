{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Data.Stage where

import Prologue

import qualified Data.Graph.Data.Graph.Class    as Graph
import qualified Luna.IR                        as IR
import qualified Luna.Pass                      as Pass
import qualified Luna.Pass.Scheduler            as Scheduler
import qualified Luna.Pass.Data.Layer.Requester as Requester
import qualified Luna.Pass.Data.Error           as Error

import Luna.Pass.Data.Layer.NodeMeta         (Meta)
import Luna.Pass.Data.Layer.PortMarker       (PortMarker)
import Luna.Pass.Data.Layer.SpanLength       (SpanLength)
import Luna.Pass.Data.Layer.SpanOffset       (SpanOffset)
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)

data Stage

type instance Graph.Components      Stage          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers Stage IR.Links = '[IR.Target, IR.Source, SpanOffset]
type instance Graph.ComponentLayers Stage IR.Terms
   = '[IR.Users, IR.Model, IR.Type, CodeSpan, Requester.Requester, Requester.ArisingFrom,
       Meta, PortMarker, SpanLength, Error.Error]

type Monad = Scheduler.SchedulerT (Graph.GraphT Stage IO)
type Pass  = Pass.Pass Stage

