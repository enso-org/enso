package org.enso.interpreter.instrument.command

import java.io.IOException
import java.util.logging.Level

import org.enso.compiler.context.SuggestionBuilder
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.runtime.Runtime.Api.SuggestionsDatabaseReIndexNotification

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

/**
  * A command that creates the index of modules in the compiler scope.
  *
  * @param maybeRequestId an option with request id
  * @param request a request to create the modules index
  */
class CreateModulesIndexCmd(
  maybeRequestId: Option[Api.RequestId],
  val request: Api.CreateModulesIndexRequest
) extends Command(maybeRequestId) {

  /**
    * Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      val modulesInScope =
        ctx.executionService.getContext.getTopScope.getModules.asScala
      modulesInScope.foreach(compile)
      val updates = modulesInScope.flatMap(analyze)
      reply(Api.CreateModulesIndexResponse(updates))
    }
  }

  /**
    * Compile the module.
    *
    * @param module the module to compile.
    * @param ctx the runtime context
    * @return the compiled module
    */
  private def compile(module: Module)(implicit ctx: RuntimeContext): Module = {
    module.compileScope(ctx.executionService.getContext)
    ctx.executionService.getLogger
      .finest(s"Compiled ${module.getName} to ${module.getCompilationStage}")
    module
  }

  private def analyze(module: Module)(implicit
    ctx: RuntimeContext
  ): Option[SuggestionsDatabaseReIndexNotification] = {
    try module.getSource
    catch {
      case e: IOException =>
        ctx.executionService.getLogger.log(
          Level.SEVERE,
          s"Failed to get module source to analyze ${module.getName}",
          e
        )
    }
    if (!module.isIndexed && module.getLiteralSource != null) {
      ctx.executionService.getLogger.finest(s"Analyzing ${module.getName}")
      val moduleName = module.getName.toString
      val addedSuggestions = SuggestionBuilder(module.getLiteralSource)
        .build(moduleName, module.getIr)
      module.setIndexed(true)
      Some(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          addedSuggestions.map(Api.SuggestionsDatabaseUpdate.Add)
        )
      )
    } else {
      None
    }
  }

}
