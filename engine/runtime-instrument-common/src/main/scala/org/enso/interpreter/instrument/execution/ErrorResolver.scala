package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.interop.InteropLibrary
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceNode

import java.io.File
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
    val iop = InteropLibrary.getUncached
    val arr = GetStackTraceNode.stackTraceToArray(iop, throwable)
    val len = iop.getArraySize(arr)
    val stackWithOptions = for (i <- 0L until len) yield {
      val elem = iop.readArrayElement(arr, i)
      toStackElement(iop, elem)
    }
    val stack = stackWithOptions.map(op => op.get)
    stack.toVector
  }

  /** Convert from the truffle stack element to the runtime API representation.
    *
    * @param iop interop library to use
    * @param element the trufle stack trace element
    * @param ctx the runtime context
    * @return the runtime API representation of the stack trace element
    */
  private def toStackElement(
    iop: InteropLibrary,
    element: Any
  )(implicit ctx: RuntimeContext): Option[Api.StackTraceElement] = {
    if (!iop.hasExecutableName(element)) {
      None
    } else {
      val name = iop.asString(iop.getExecutableName(element))
      if (!iop.hasSourceLocation(element)) {
        Some(Api.StackTraceElement(name, None, None, None))
      } else {
        val section = iop.getSourceLocation(element)
        if (section.getSource.isInternal) {
          None
        } else {
          Some(
            Api.StackTraceElement(
              name,
              findFileByModuleName(section.getSource.getName),
              Some(LocationResolver.sectionToRange(section)),
              LocationResolver.getExpressionId(section).map(_.externalId)
            )
          )
        }
      }
    }
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
