package org.enso.compiler.pass.analyse

import org.enso.compiler.pass.analyse.FramePointer
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.Name.GenericAnnotation
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.core.ir.expression.{Application, Case}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name,
  Pattern,
  Type
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.alias.AliasMetadata
import org.enso.compiler.pass.analyse.alias.graph.{Graph, GraphOccurrence}

/** This pass attaches [[FramePointer]] as metadata to all the IR elements that already
  * have [[org.enso.compiler.pass.analyse.alias.AliasMetadata.Occurrence]] attached.
  * It does not replace the IR elements with errors, it just attaches metadata.
  */
case object FramePointerAnalysis extends IRPass {

  override type Metadata = FrameAnalysisMeta

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = {
    Seq(AliasAnalysis)
  }

  override val invalidatedPasses: Seq[IRPass] = Seq(this)

  override def runModule(ir: Module, moduleContext: ModuleContext): Module = {
    ir.bindings.foreach(processBinding)
    ir
  }

  private def processBinding(
    ir: Definition
  ): Unit = {
    ir match {
      case m: Method.Explicit =>
        getAliasAnalysisGraph(m) match {
          case Some(
                graph
              ) =>
            processExpression(m.body, graph)
            FrameAnalysisMeta.updateSymbolNames(m, graph.rootScope)
          case _ => ()
        }
      case m: Method.Conversion =>
        getAliasAnalysisGraph(m) match {
          case Some(graph) =>
            processExpression(m.body, graph)
            FrameAnalysisMeta.updateSymbolNames(m, graph.rootScope)
          case _ => ()
        }
      case t: Definition.Type =>
        getAliasAnalysisGraph(t) match {
          case Some(graph) =>
            processArgumentDefs(t.params, graph)
            t.members.foreach { member =>
              val memberGraph = getAliasRootScope(member) match {
                case Some(memberRootScope) =>
                  memberRootScope.graph
                case _ => graph
              }
              processArgumentDefs(member.arguments, memberGraph)
              member.annotations.foreach { annotation =>
                processAnnotation(annotation, memberGraph)
              }
              FrameAnalysisMeta.updateSymbolNames(member, memberGraph.rootScope)
            }
            FrameAnalysisMeta.updateSymbolNames(t, graph.rootScope)
          case _ => ()
        }
      case annot: GenericAnnotation =>
        getAliasAnalysisGraph(annot) match {
          case Some(annotGraph) =>
            processAnnotation(annot, annotGraph)
          case None =>
            throw new CompilerError(
              s"No alias analysis graph found for annotation $annot"
            )
        }
      case _ => ()
    }
  }

  private def processAnnotation(
    annot: GenericAnnotation,
    graph: Graph
  ): Unit = {
    val annotGraph = getAliasRootScope(annot) match {
      case Some(rootScope) =>
        rootScope.graph
      case None => graph
    }
    FrameAnalysisMeta.updateSymbolNames(annot, annotGraph.rootScope)
    processExpression(annot.expression, annotGraph)
  }

  private def processArgumentDefs(
    args: List[DefinitionArgument],
    graph: Graph
  ): Unit = {
    args.foreach { arg =>
      arg.name match {
        case Name.Self(loc, synthetic, _) if loc == null && synthetic =>
          // synthetic self argument has occurrence attached, but there is no Occurence.Def for it.
          // So we have to handle it specially.
          FrameAnalysisMeta.updateMetadata(arg, new FramePointer(0, 1))
        case _ =>
          maybeAttachFramePointer(arg, graph)
      }
      arg.defaultValue match {
        case Some(defaultValue) =>
          getAliasAnalysisGraph(defaultValue) match {
            case Some(defaultValueGraph) =>
              processExpression(defaultValue, defaultValueGraph, false)
              maybAttachFrameVariableNames(defaultValue)
            case None =>
              processExpression(defaultValue, graph)
          }
        case None => ()
      }
    }
  }

  private def processExpression(
    exprIr: Expression,
    graph: Graph,
    updateSymbols: Boolean = true
  ): Unit = {
    exprIr match {
      case name: Name => maybeAttachFramePointer(name, graph)
      case block: Expression.Block =>
        block.expressions.foreach { blockExpr =>
          processExpression(blockExpr, graph)
        }
        processExpression(block.returnValue, graph)
      case lam: Function.Lambda =>
        processArgumentDefs(lam.arguments(), graph)
        processExpression(lam.body(), graph)
      case binding @ Expression.Binding(name, expr, _, _) =>
        maybeAttachFramePointer(name, graph)
        processExpression(expr, graph)
        maybeAttachFramePointer(binding, graph)
      case app: Application => processApplication(app, graph)
      case caseExpr: Case.Expr =>
        processExpression(caseExpr.scrutinee, graph)
        caseExpr.branches.foreach { branch =>
          processCaseBranch(branch)
        }
      case asc: Type.Ascription =>
        processExpression(asc.typed, graph)
        processExpression(asc.signature, graph)
      case _ => ()
    }
    if (updateSymbols) {
      maybAttachFrameVariableNames(exprIr)
    }
  }

  private def processCaseBranch(
    branch: Case.Branch
  ): Unit = {
    getAliasAnalysisGraph(branch) match {
      case None =>
        throw new CompilerError(
          "An alias analysis graph is expected on " + branch
        )
      case Some(graph) =>
        maybAttachFrameVariableNames(branch)
        processExpression(branch.expression, graph)
        processCasePattern(branch.pattern, graph)
    }
  }

  /** @param graph Graph fetched from the corresponding Case.Branch
    */
  private def processCasePattern(
    pattern: Pattern,
    graph: Graph
  ): Unit = {
    pattern match {
      case name: Pattern.Name =>
        processExpression(name.name, graph)
      case lit: Pattern.Literal =>
        processExpression(lit.literal, graph)
      case tp: Pattern.Type =>
        processExpression(tp.name, graph)
        processExpression(tp.tpe, graph)
      case ctor: Pattern.Constructor =>
        processExpression(ctor.constructor, graph)
        ctor.fields.foreach { field =>
          processCasePattern(field, graph)
        }
      case _: Pattern.Documentation => ()
      case _                        => ()
    }
    FrameAnalysisMeta.updateSymbolNames(pattern, graph.rootScope)
  }

  private def processApplication(
    application: Application,
    graph: Graph
  ): Unit = {
    application match {
      case app: Application.Prefix =>
        maybeAttachFramePointer(app, graph)
        processExpression(app.function, graph)
        processCallArguments(app.arguments, graph)
      case force: Application.Force =>
        processExpression(force.target, graph)
      case seq: Application.Sequence =>
        seq.items.foreach { item =>
          processExpression(item, graph)
        }
      case tSet: Application.Typeset =>
        tSet.expression.foreach(processExpression(_, graph))
      case _ =>
        throw new CompilerError(
          "Unexpected type of Application: " + application
        )
    }
  }

  private def processCallArguments(
    arguments: List[CallArgument],
    graph: Graph
  ): Unit = {
    arguments.foreach { case arg: CallArgument.Specified =>
      maybeAttachFramePointer(arg, graph)
      arg.name.foreach(maybeAttachFramePointer(_, graph))
      processExpression(arg.value, graph, false)
      maybAttachFrameVariableNames(arg.value)
      maybAttachFrameVariableNames(arg)
    }
  }

  private def maybAttachFrameVariableNames(ir: IR): Unit = {
    getAliasRootScope(ir).foreach(root =>
      FrameAnalysisMeta.updateSymbolNames(ir, root.graph.rootScope)
    )
    getAliasChildScope(ir).foreach(child =>
      FrameAnalysisMeta.updateSymbolNames(ir, child.scope)
    )
  }

  /** Attaches [[FramePointerMeta]] metadata to the given `ir` if there is an
    * appropriate [[AliasMetadata.Occurrence]] already attached to it.
    * @param ir IR to attach the frame pointer metadata to.
    * @param graph Alias analysis graph
    * @return Copy of `ir` with attached metadata, or just the `ir` if nothing
    *         was attached.
    */
  private def maybeAttachFramePointer(
    ir: IR,
    graph: Graph
  ): Unit = {
    getAliasAnalysisMeta(ir) match {
      case Some(occ: AliasMetadata.Occurrence) =>
        val id = occ.id()
        graph.scopeFor(id) match {
          case Some(scope) =>
            graph.getOccurrence(id) match {
              case Some(occ: GraphOccurrence) =>
                FrameAnalysisMeta.updateOccurrance(ir, graph, scope, occ)
              case _ => ()
            }
          case _ => ()
        }
      case _ => ()
    }
  }

  private def getAliasAnalysisMeta(
    ir: IR
  ): Option[AliasAnalysis.Metadata] = {
    ir.passData.get(AliasAnalysis) match {
      case Some(aliasInfo: AliasMetadata) =>
        Some(aliasInfo)
      case _ => None
    }
  }

  private def getAliasRootScope(
    ir: IR
  ): Option[AliasMetadata.RootScope] = {
    ir.passData().get(AliasAnalysis) match {
      case Some(root: AliasMetadata.RootScope) => Some(root)
      case _                                   => None
    }
  }

  private def getAliasChildScope(
    ir: IR
  ): Option[AliasMetadata.ChildScope] = {
    ir.passData()
      .get(AliasAnalysis)
      .filter(_.isInstanceOf[AliasMetadata.ChildScope])
      .map(_.asInstanceOf[AliasMetadata.ChildScope])
  }

  private def getAliasAnalysisGraph(
    ir: IR
  ): Option[Graph] = {
    getAliasAnalysisMeta(ir).map(_.graph)
  }

  /** @inheritdoc
    */
  override def runExpression(
    exprIr: Expression,
    inlineContext: InlineContext
  ): Expression = {
    inlineContext.localScope match {
      case None =>
        throw new CompilerError(
          "Local scope must be provided for frame pointer analysis"
        )
      case Some(localScope) =>
        val graph = localScope.aliasingGraph()
        processExpression(exprIr, graph)
        exprIr
    }
  }
}
