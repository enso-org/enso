package org.enso.interpreter.instrument.execution

import java.io.File

import com.oracle.truffle.api.{
  TruffleException,
  TruffleStackTrace,
  TruffleStackTraceElement
}
import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.polyglot.LanguageInfo
import org.enso.polyglot.runtime.Runtime.Api

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional

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

  /** Create a stack trace of a guest language from a java exception.
    *
    * @param throwable the exception
    * @param ctx the runtime context
    * @return a runtime API representation of a stack trace
    */
  def getStackTrace(
    throwable: Throwable
  )(implicit ctx: RuntimeContext): Vector[Api.StackTraceElement] =
    TruffleStackTrace
      .getStackTrace(throwable)
      .asScala
      .map(toStackElement)
      .toVector

  /** Convert from the truffle stack element to the runtime API representation.
    *
    * @param element the trufle stack trace element
    * @param ctx the runtime context
    * @return the runtime API representation of the stack trace element
    */
  private def toStackElement(
    element: TruffleStackTraceElement
  )(implicit ctx: RuntimeContext): Api.StackTraceElement = {
    val node = element.getLocation
    node.getEncapsulatingSourceSection match {
      case null =>
        Api.StackTraceElement(node.getRootNode.getName, None, None, None)
      case section =>
        Api.StackTraceElement(
          element.getTarget.getRootNode.getName,
          findFileByModuleName(section.getSource.getName),
          Some(LocationResolver.sectionToRange(section)),
          LocationResolver.getExpressionId(section).map(_.externalId)
        )
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

  /** Find source file path by the module name.
    *
    * @param module the module name
    * @param ctx the runtime context
    * @return the source file path
    */
  private def findFileByModuleName(
    module: String
  )(implicit ctx: RuntimeContext): Option[File] =
    for {
      module <- ctx.executionService.getContext.findModule(module).toScala
      path   <- Option(module.getPath)
    } yield new File(path)

}
