package org.enso.interpreter.instrument.job

import org.enso.compiler.context.{
  Changeset,
  ExportsBuilder,
  ModuleExportsDiff,
  SuggestionBuilder,
  SuggestionDiff
}
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.ModuleExports
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.buffer.Rope

import java.util.logging.Level

final class AnalyzeModuleJob(module: Module, changeset: Changeset[Rope])
    extends BackgroundJob[Unit](AnalyzeModuleJob.Priority) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    AnalyzeModuleJob.analyzeModule(module, changeset)
  }

  override def toString: String =
    s"${getClass.getSimpleName}(${module.getName}, ...)"
}

object AnalyzeModuleJob {

  def apply(module: Module, changeset: Changeset[Rope]): AnalyzeModuleJob =
    new AnalyzeModuleJob(module, changeset)

  private val Priority = 10

  private val exportsBuilder = new ExportsBuilder

  def analyzeModule(
    module: Module,
    changeset: Changeset[Rope]
  )(implicit ctx: RuntimeContext): Unit = {
    if (ctx.executionService.getContext.isProjectSuggestionsEnabled) {
      doAnalyzeModule(module, changeset)
    }
  }

  private def doAnalyzeModule(
    module: Module,
    changeset: Changeset[Rope]
  )(implicit ctx: RuntimeContext): Unit = {
    val moduleName = module.getName
    if (module.isIndexed) {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing indexed module $moduleName")
      val prevSuggestions = SuggestionBuilder(changeset.source)
        .build(moduleName, changeset.ir)
      val newSuggestions =
        SuggestionBuilder(module.getSource.getCharacters)
          .build(moduleName, module.getIr)
      val diff = SuggestionDiff
        .compute(prevSuggestions, newSuggestions)
      val prevExports = exportsBuilder.build(moduleName, changeset.ir)
      val newExports  = exportsBuilder.build(moduleName, module.getIr)
      val exportsDiff = ModuleExportsDiff.compute(prevExports, newExports)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = moduleName.toString,
        actions = Vector(),
        exports = exportsDiff,
        updates = diff
      )
      sendModuleUpdate(notification)
    } else {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing not-indexed module ${module.getName}")
      val newSuggestions =
        SuggestionBuilder(module.getSource.getCharacters)
          .build(moduleName, module.getIr)
      val prevExports = ModuleExports(moduleName.toString, Set())
      val newExports  = exportsBuilder.build(moduleName, module.getIr)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        module = moduleName.toString,
        actions =
          Vector(Api.SuggestionsDatabaseAction.Clean(moduleName.toString)),
        exports = ModuleExportsDiff.compute(prevExports, newExports),
        updates = SuggestionDiff.compute(Tree.empty, newSuggestions)
      )
      sendModuleUpdate(notification)
      module.setIndexed(true)
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
