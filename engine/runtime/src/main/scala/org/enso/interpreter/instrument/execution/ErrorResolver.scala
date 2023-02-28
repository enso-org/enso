package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.{TruffleStackTrace, TruffleStackTraceElement}
import org.enso.polyglot.runtime.Runtime.Api

import java.io.File
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional

/** Methods for handling exceptions in the interpreter. */
object ErrorResolver {

  /** Create a stack trace of a guest language from a java exception.
    *
    * @param throwable the exception
    * @param ctx the runtime context
    * @return a runtime API representation of a stack trace
    */
  def getStackTrace(
    throwable: Throwable
  )(implicit ctx: RuntimeContext): Vector[Api.StackTraceElement] = {
    TruffleStackTrace
      .getStackTrace(throwable)
      .asScala
      .flatMap(toStackElement)
      .toVector
  }

  /** Convert from the truffle stack element to the runtime API representation.
    *
    * @param element the trufle stack trace element
    * @param ctx the runtime context
    * @return the runtime API representation of the stack trace element
    */
  private def toStackElement(
    element: TruffleStackTraceElement
  )(implicit ctx: RuntimeContext): Option[Api.StackTraceElement] = {
    val node = Option(element.getLocation)
    node.flatMap(x => {
      x.getEncapsulatingSourceSection match {
        case null if x.getRootNode == null =>
          None
        case null =>
          Some(Api.StackTraceElement(x.getRootNode.getName, None, None, None))
        case section =>
          Some(
            Api.StackTraceElement(
              element.getTarget.getRootNode.getName,
              findFileByModuleName(section.getSource.getName),
              Some(LocationResolver.sectionToRange(section)),
              LocationResolver.getExpressionId(section).map(_.externalId)
            )
          )
      }
    })
  }

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
