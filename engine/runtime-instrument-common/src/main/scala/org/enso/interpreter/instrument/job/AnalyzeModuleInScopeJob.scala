package org.enso.interpreter.instrument.job

import org.enso.compiler.suggestions.{
  ExportsBuilder,
  ModuleExportsDiff,
  SuggestionBuilder,
  SuggestionDiff
}
import org.enso.interpreter.instrument.execution.ModuleIndexing.IndexState
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.{ModuleExports, Suggestion}

import java.util.logging.Level

final class AnalyzeModuleInScopeJob(
  modules: Iterable[(Module, IndexState, Boolean)]
) extends BackgroundJob[Unit](AnalyzeModuleInScopeJob.Priority) {

  private val exportsBuilder = new ExportsBuilder

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Unit = {
    // There are two runtime flags that can disable suggestions for project
    // and global modules (libraries). They are used primarily in tests to
    // disable the suggestion updates and reduce the number of messages that
    // runtime sends.
    if (ctx.executionService.getContext.isProjectSuggestionsEnabled) {
      modules.foreach((analyzeModuleInScope _).tupled)
      ctx.endpoint.sendToClient(
        Api.Response(Api.AnalyzeModuleInScopeJobFinished())
      )
    }
  }

  override def toString: String =
    s"AnalyzeModuleInScopeJob($modules)"

  private def analyzeModuleInScope(
    module: Module,
    state: IndexState,
    hasSource: Boolean
  )(implicit
    ctx: RuntimeContext
  ): Unit = {
    if (!state.isIndexed && hasSource) {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing module in scope {0}", module.getName)
      val moduleName = module.getName
      val compiler   = ctx.executionService.getContext.getCompiler
      val types      = Module.findTypeHierarchy(compiler.context)
      val newSuggestions =
        SuggestionBuilder(
          module.asCompilerModule(),
          types,
          compiler
        )
          .build(moduleName, state.ir)
          .filter(Suggestion.isGlobal)
      val prevExports = ModuleExports(moduleName.toString, Set())
      val newExports  = exportsBuilder.build(module.getName, state.ir)
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
            s"Calculated index for module in scope {0} is not up-to-date. Discarding",
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

object AnalyzeModuleInScopeJob {

  /** Create an instance of [[AnalyzeModuleInScopeJob]].
    *
    * @param modules the list of modules to analyze
    * @return the [[AnalyzeModuleInScopeJob]]
    */
  def apply(
    modules: Iterable[(Module, IndexState, Boolean)]
  ): AnalyzeModuleInScopeJob =
    new AnalyzeModuleInScopeJob(modules)

  private val Priority = 11
}
