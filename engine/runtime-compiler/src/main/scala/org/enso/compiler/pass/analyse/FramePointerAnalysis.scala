package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{
  CompilerContext,
  FramePointer,
  InlineContext,
  LocalScope,
  ModuleContext
}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name,
  ProcessingPass
}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRPass.IRMetadata
import org.enso.compiler.pass.analyse.alias.{Graph, Info}

/** This pass attaches [[FramePointer]] as metadata to all the IR elements that already
  * have [[org.enso.compiler.pass.analyse.alias.Info.Occurrence]] attached.
  */
case object FramePointerAnalysis extends IRPass {

  override type Metadata = FramePointerMeta

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = {
    Seq(AliasAnalysis)
  }

  override val invalidatedPasses: Seq[IRPass] = Seq(this)

  override def runModule(ir: Module, moduleContext: ModuleContext): Module = {
    val newBindings = ir.bindings.map(processBinding)
    ir.copy(bindings = newBindings)
  }

  private def processBinding(
    ir: Definition
  ): Definition = {
    ir match {
      case m: Method.Explicit =>
        getAliasAnalysisGraph(m) match {
          case Some(graph) =>
            m.copy(
              body = m.body.mapExpressions(processExpression(_, graph))
            )
          case _ => m
        }
      case m: Method.Conversion =>
        getAliasAnalysisGraph(m) match {
          case Some(graph) =>
            m.copy(
              body = m.body.mapExpressions(processExpression(_, graph))
            )
          case _ => m
        }
      case t: Definition.Type =>
        getAliasAnalysisGraph(t) match {
          case Some(graph) =>
            t.copy(
              params = processArgumentDefs(
                t.params,
                graph
              ),
              members = t.members.map(d => {
                d.copy(
                  arguments = processArgumentDefs(
                    d.arguments,
                    graph
                  ),
                  annotations = d.annotations.map { ann =>
                    ann.copy(
                      expression = processExpression(
                        ann.expression,
                        graph
                      )
                    )
                  }
                )
              })
            )
          case _ => t
        }
      case _ => ir
    }
  }

  private def processArgumentDefs(
    args: List[DefinitionArgument],
    graph: Graph
  ): List[DefinitionArgument] = {
    args.map {
      case arg @ DefinitionArgument.Specified(
            name,
            ascribedType,
            defaultValue,
            _,
            _,
            _,
            _
          ) =>
        arg.copy(
          name         = maybeAttachFramePointer(name, graph),
          ascribedType = ascribedType.map(processExpression(_, graph)),
          defaultValue = defaultValue.map(processExpression(_, graph))
        )
    }
  }

  private def processExpression(
    exprIr: Expression,
    graph: Graph
  ): Expression = {
    exprIr match {
      case name: Name => maybeAttachFramePointer(name, graph)
      case block: Expression.Block =>
        block.copy(
          expressions = block.expressions.map { expr =>
            processExpression(expr, graph)
          },
          returnValue = processExpression(block.returnValue, graph)
        )
      case lambda @ Function.Lambda(args, body, _, _, _, _) =>
        lambda.copy(
          arguments = processArgumentDefs(args, graph),
          body      = processExpression(body, graph)
        )
      case binding @ Expression.Binding(name, expr, _, _, _) =>
        maybeAttachFramePointer(binding, graph)
          .copy(
            name       = maybeAttachFramePointer(name, graph),
            expression = processExpression(expr, graph)
          )
      case app: Application => processApplication(app, graph)
      case _ =>
        exprIr.mapExpressions(processExpression(_, graph))
    }
  }

  private def processApplication(
    application: Application,
    graph: Graph
  ): Application = {
    application match {
      case app @ Application.Prefix(func, arguments, _, _, _, _) =>
        app.copy(
          function  = processExpression(func, graph),
          arguments = processCallArguments(arguments, graph)
        )
      case app @ Application.Force(expr, _, _, _) =>
        app.copy(target = processExpression(expr, graph))
      case app @ Application.Sequence(items, _, _, _) =>
        app.copy(items = items.map(processExpression(_, graph)))
      case tSet @ Application.Typeset(expr, _, _, _) =>
        tSet.copy(
          expression = expr.map(processExpression(_, graph))
        )
      case _ =>
        throw new CompilerError(
          "Unexpected type of Application: " + application
        )
    }
  }

  private def processCallArguments(
    arguments: List[CallArgument],
    graph: Graph
  ): List[CallArgument] = {
    arguments.map { case arg @ CallArgument.Specified(name, value, _, _, _) =>
      arg.copy(
        name  = name.map(maybeAttachFramePointer(_, graph)),
        value = processExpression(value, graph)
      )
    }
  }

  /** Attaches [[FramePointerMeta]] metadata to the given `ir` if there is an
    * appropriate [[Info.Occurrence]] already attached to it.
    * @param ir IR to attach the frame pointer metadata to.
    * @param graph Alias analysis graph
    * @tparam T Type of IR.
    * @return Copy of `ir` with attached metadata, or just the `ir` if nothing
    *         was attached.
    */
  private def maybeAttachFramePointer[T <: IR](
    ir: T,
    graph: Graph
  ): T = {
    getAliasAnalysisMeta(ir) match {
      case Some(Info.Occurrence(_, id)) =>
        graph.scopeFor(id) match {
          case Some(scope) =>
            graph.getOccurrence(id) match {
              case Some(use: Graph.Occurrence.Use) =>
                // Use is allowed to read a variable from some parent scope
                graph.defLinkFor(use.id) match {
                  case Some(defLink) =>
                    val defId = defLink.target
                    val defOcc = graph
                      .getOccurrence(defId)
                      .get
                      .asInstanceOf[Graph.Occurrence.Def]
                    val defScope    = graph.scopeFor(defId).get
                    val parentLevel = getScopeDistance(defScope, scope)
                    val frameSlotIdx =
                      getFrameSlotIdxInScope(graph, defScope, defOcc)
                    ir.updateMetadata(
                      new MetadataPair(
                        this,
                        new FramePointerMeta(
                          new FramePointer(parentLevel, frameSlotIdx)
                        )
                      )
                    )
                  case None =>
                    // It is possible that there is no Def for this Use. It can, for example, be
                    // Use for some global symbol. In `IrToTruffle`, an UnresolvedSymbol will be
                    // generated for it.
                    // We will not attach any metadata in this case.
                    ir
                }
              case Some(defn: Graph.Occurrence.Def) =>
                // The definition cannot write to parent's frame slots.
                val parentLevel  = 0
                val frameSlotIdx = getFrameSlotIdxInScope(graph, scope, defn)
                ir.updateMetadata(
                  new MetadataPair(
                    this,
                    new FramePointerMeta(
                      new FramePointer(parentLevel, frameSlotIdx)
                    )
                  )
                )
              case _ => ir
            }
          case _ => ir
        }
      case _ => ir
    }
  }

  /** Returns the index of the given `defOcc` definition in the given `scope`
    * @param scope This scope must contain the given `defOcc`
    * @param defOcc This occurrence must be in the given `scope`
    */
  private def getFrameSlotIdxInScope(
    graph: Graph,
    scope: Graph.Scope,
    defOcc: Graph.Occurrence.Def
  ): Int = {
    assert(
      graph.scopeFor(defOcc.id).contains(scope),
      "Def occurrence must be in the given scope"
    )
    assert(
      scope.allDefinitions.contains(defOcc),
      "The given scope must contain the given Def occurrence"
    )
    val idxInScope = scope.allDefinitions.zipWithIndex
      .find { case (def_, _) => def_.id == defOcc.id }
      .map(_._2)
      .getOrElse(
        throw new IllegalStateException(
          "Def occurrence must be in the given scope"
        )
      )
    idxInScope + LocalScope.internalSlotsSize
  }

  /** Returns the *scope distance* of the given `childScope` to the given `parentScope`.
    * Scope distance is the number of parents from the `childScope`.
    * @param parentScope Some of the parent scopes of `childScope`.
    * @param childScope Nested child scope of `parentScope`.
    * @return
    */
  private def getScopeDistance(
    parentScope: Graph.Scope,
    childScope: Graph.Scope
  ): Int = {
    var currScope: Option[Graph.Scope] = Some(childScope)
    var scopeDistance                  = 0
    while (currScope.isDefined && currScope.get != parentScope) {
      currScope = currScope.get.parent
      scopeDistance += 1
    }
    scopeDistance
  }

  private def getAliasAnalysisMeta(
    ir: IR
  ): Option[AliasAnalysis.Metadata] = {
    ir.passData.get(AliasAnalysis) match {
      case Some(aliasInfo: Info) =>
        Some(aliasInfo)
      case _ => None
    }
  }

  private def getAliasAnalysisGraph(
    ir: IR
  ): Option[Graph] = {
    getAliasAnalysisMeta(ir).map(_.graph)
  }

  /** Not implemented for this pass.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    ir
  }

  // === Pass Configuration ===================================================

  class FramePointerMeta(
    val framePointer: FramePointer
  ) extends IRMetadata {
    override val metadataName: String = "FramePointer"

    /** @inheritdoc
      */
    override def duplicate(): Option[Metadata] = {
      Some(new FramePointerMeta(framePointer))
    }

    /** @inheritdoc
      */
    override def prepareForSerialization(
      compiler: CompilerContext
    ): ProcessingPass.Metadata = this

    /** @inheritdoc
      */
    override def restoreFromSerialization(
      compiler: CompilerContext
    ): Option[ProcessingPass.Metadata] = Some(this)
  }
}
