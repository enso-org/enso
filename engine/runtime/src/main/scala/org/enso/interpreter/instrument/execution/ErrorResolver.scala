package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.TruffleException
import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.polyglot.LanguageInfo
import org.enso.polyglot.runtime.Runtime.Api

/** Methods for handling exceptions in the interpreter. */
object ErrorResolver {

  /** Create expression updates about the failed expression and expressions
    * that were not executed (poisoned) due to the failed expression.
    *
    * @param error the runtime exception
    * @param ctx the runtime context
    * @return the list of updates about the expressions not executed
    */
  def createUpdates(error: Throwable)(implicit
    ctx: RuntimeContext
  ): Set[Api.ExpressionUpdate] = {
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
                val poisoned: Set[Api.ExpressionUpdate] = meta
                  .getExternal(toDataflowDependencyType(expressionId))
                  .getOrElse(Set())
                  .map { id =>
                    Api.ExpressionUpdate(
                      id,
                      None,
                      None,
                      Vector(Api.ProfilingInfo.ExecutionTime(0)),
                      false,
                      Api.ExpressionUpdate.Payload.Poisoned(Seq())
                    )
                  }
                val failed =
                  Api.ExpressionUpdate(
                    expressionId.externalId,
                    None,
                    None,
                    Vector(Api.ProfilingInfo.ExecutionTime(0)),
                    false,
                    Api.ExpressionUpdate.Payload
                      .RuntimeError(error.getMessage, Seq())
                  )
                poisoned + failed
              }
              .getOrElse(Set())
          }
          .orElse(Set())
      case None =>
        Set()
    }
  }

  /** Get the source location of the runtime exception.
    *
    * @param error the runtime exception
    * @return the error location in the source file
    */
  private def getErrorSource(error: Throwable): Option[SourceSection] =
    error match {
      case ex: TruffleException
          if getLanguage(ex).forall(_ == LanguageInfo.ID) =>
        Option(ex.getSourceLocation)
      case _ =>
        None
    }

  /** Get the language produced the runtime exception.
    *
    * @param ex the runtime exception
    * @return the language of the source file produced the runtime exception
    */
  private def getLanguage(ex: TruffleException): Option[String] =
    for {
      location <- Option(ex.getSourceLocation)
      source   <- Option(location.getSource)
    } yield source.getLanguage

  /** Convert this expression id to the dataflow dependency type. */
  private def toDataflowDependencyType(
    id: LocationResolver.ExpressionId
  ): DataflowAnalysis.DependencyInfo.Type.Static =
    DataflowAnalysis.DependencyInfo.Type
      .Static(id.internalId, Some(id.externalId))

  val DependencyFailed: String =
    "Dependency failed."
}
