package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.TruffleException
import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.polyglot.LanguageInfo
import org.enso.polyglot.runtime.Runtime.Api

object ErrorHandler {

  def createUpdates(error: Throwable)(implicit
    ctx: RuntimeContext
  ): Seq[Api.ExpressionUpdate] = {
    getErrorSource(error) match {
      case Some(section) =>
        val moduleName = section.getSource.getName
        val moduleOpt  = ctx.executionService.getContext.findModule(moduleName)
        moduleOpt
          .map { module =>
            val meta = module.getIr.unsafeGetMetadata(
              DataflowAnalysis,
              "Empty dataflow analysis metadata during program execution."
            )
            LocationResolver
              .getExpressionId(section, module)
              .map { expressionId =>
                val poisoned = meta
                  .getExternal(toDataflowDependencyType(expressionId))
                  .getOrElse(Set())
                  .map(
                    Api.ExpressionUpdate
                      .ExpressionPoisoned(_, expressionId.externalId)
                  )
                  .toSeq
                val failed =
                  Api.ExpressionUpdate
                    .ExpressionFailed(expressionId.externalId, error.getMessage)
                failed +: poisoned
              }
              .getOrElse(Seq())
          }
          .orElse(Seq())
      case None =>
        Seq()
    }
  }

  private def getErrorSource(error: Throwable): Option[SourceSection] =
    error match {
      case ex: TruffleException
          if getLanguage(ex).forall(_ == LanguageInfo.ID) =>
        Option(ex.getSourceLocation)
      case _ =>
        None
    }

  private def getLanguage(ex: TruffleException): Option[String] =
    for {
      location <- Option(ex.getSourceLocation)
      source   <- Option(location.getSource)
    } yield source.getLanguage

  private def toDataflowDependencyType(
    id: LocationResolver.ExpressionId
  ): DataflowAnalysis.DependencyInfo.Type.Static =
    DataflowAnalysis.DependencyInfo.Type
      .Static(id.internalId, Some(id.externalId))

}
