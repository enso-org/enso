package org.enso.interpreter.instrument.execution

import java.io.File

import com.oracle.truffle.api.{
  TruffleException,
  TruffleStackTrace,
  TruffleStackTraceElement
}
import org.enso.polyglot.runtime.Runtime.Api

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

  /** Get the language produced the runtime exception.
    *
    * @param ex the runtime exception
    * @return the language of the source file produced the runtime exception
    */
  def getLanguage(ex: TruffleException): Option[String] =
    for {
      location <- Option(ex.getSourceLocation)
      source   <- Option(location.getSource)
    } yield source.getLanguage

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
