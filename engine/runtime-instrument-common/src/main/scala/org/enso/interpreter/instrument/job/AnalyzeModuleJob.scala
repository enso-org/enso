package org.enso.interpreter.instrument.job

import org.enso.compiler.suggestions.{
  ExportsBuilder,
  ModuleExportsDiff,
  SuggestionBuilder,
  SuggestionDiff
}
import org.enso.compiler.core.IR
import org.enso.interpreter.instrument.Changeset
import org.enso.interpreter.instrument.execution.ModuleIndexing.IndexState
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.ModuleExports
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.buffer.Rope

import java.util.logging.Level

final class AnalyzeModuleJob(
  module: Module,
  state: IndexState,
  ir: IR,
  changeset: Changeset[Rope]
) extends BackgroundJob[Unit](AnalyzeModuleJob.Priority) {

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Unit = {
    AnalyzeModuleJob.analyzeModule(module, state, ir, changeset)
  }

  override def toString: String =
    s"${getClass.getSimpleName}(${module.getName}, ...)"
}

object AnalyzeModuleJob {

  def apply(
    module: Module,
    state: IndexState,
    ir: IR,
    changeset: Changeset[Rope]
  ): AnalyzeModuleJob =
    new AnalyzeModuleJob(module, state, ir, changeset)

  private val Priority = 10

  private val exportsBuilder = new ExportsBuilder

  def analyzeModule(
    module: Module,
    state: IndexState,
    newIr: IR,
    changeset: Changeset[Rope]
  )(implicit ctx: RuntimeContext): Unit = {
    if (ctx.executionService.getContext.isProjectSuggestionsEnabled) {
      doAnalyzeModule(module, state, newIr, changeset)
    }
  }

  private def doAnalyzeModule(
    module: Module,
    state: IndexState,
    newIr: IR,
    changeset: Changeset[Rope]
  )(implicit ctx: RuntimeContext): Unit = {
    val moduleName = module.getName
    val compiler   = ctx.executionService.getContext.getCompiler
    if (state.isIndexed) {
      ctx.executionService.getLogger
        .log(Level.FINEST, "Analyzing indexed module {0}", moduleName)
      val types = Module.findTypeHierarchy(compiler.context)
      val prevSuggestions =
        SuggestionBuilder(changeset.source, types, compiler)
          .build(moduleName, changeset.ir)
      val newSuggestions =
        SuggestionBuilder(module.asCompilerModule(), types, compiler)
          .build(moduleName, newIr)
      val diff = SuggestionDiff
        .compute(prevSuggestions, newSuggestions)
      val prevExports = exportsBuilder.build(moduleName, changeset.ir)
      val newExports  = exportsBuilder.build(moduleName, newIr)
      val exportsDiff = ModuleExportsDiff.compute(prevExports, newExports)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = moduleName.toString,
        actions = Vector(),
        exports = exportsDiff,
        updates = diff
      )
      if (ctx.state.suggestions.updateState(module, state, newIr)) {
        sendModuleUpdate(notification)
      } else {
        ctx.executionService.getLogger
          .log(
            Level.FINEST,
            s"Newly calculated index for module {0} is not up-to-date. Discarding",
            module.getName
          )
      }
    } else {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing not-indexed module {0}", module.getName)
      val types = Module.findTypeHierarchy(compiler.context)
      val newSuggestions =
        SuggestionBuilder(module.asCompilerModule(), types, compiler)
          .build(moduleName, state.ir)
      val prevExports = ModuleExports(moduleName.toString, Set())
      val newExports  = exportsBuilder.build(moduleName, state.ir)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        module = moduleName.toString,
        actions =
          Vector(Api.SuggestionsDatabaseAction.Clean(moduleName.toString)),
        exports = ModuleExportsDiff.compute(prevExports, newExports),
        updates = SuggestionDiff.compute(Tree.empty, newSuggestions)
      )
      if (ctx.state.suggestions.markAsIndexed(module, state)) {
        sendModuleUpdate(notification)
      } else {
        ctx.executionService.getLogger
          .log(
            Level.FINEST,
            s"Calculated index for module {0} is not up-to-date. Discarding",
            module.getName
          )
      }
    }
  }

  /** Send notification about module updates.
    *
    * @param payload the module update
    * @param ctx the runtime context
    */
  private def sendModuleUpdate(
    payload: Api.SuggestionsDatabaseModuleUpdateNotification
  )(implicit ctx: RuntimeContext): Unit =
    if (
      payload.actions.nonEmpty ||
      payload.exports.nonEmpty ||
      !payload.updates.isEmpty
    ) {
      ctx.endpoint.sendToClient(Api.Response(payload))
    }
}
