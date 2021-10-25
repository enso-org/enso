package org.enso.polyglot.debugger.protocol.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.polyglot.debugger.protocol.{
  ExceptionRepresentation,
  StackTraceElement
}
import java.lang.{StackTraceElement => JStackTraceElement}

object ExceptionRepresentationFactory {

  /** Creates ExceptionRepresentation inside a [[FlatBufferBuilder]].
    *
    * @param ex an exception (any Throwable) to serialize
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create(ex: Throwable)(implicit builder: FlatBufferBuilder): Int = {
    val message       = Option(ex.getMessage).orElse(Option(ex.toString))
    val messageOffset = message.map(builder.createString).getOrElse(0)

    val causeOffset =
      if (ex.getCause != null) create(ex.getCause) else 0

    val traceElements: Array[Int] = ex.getStackTrace.map(createStackTrace)
    val traceVectorOffset =
      ExceptionRepresentation.createStackTraceVector(builder, traceElements)
    ExceptionRepresentation.createExceptionRepresentation(
      builder,
      messageOffset,
      traceVectorOffset,
      causeOffset
    )
  }

  private def createStackTrace(
    traceElement: JStackTraceElement
  )(implicit builder: FlatBufferBuilder): Int = {
    val declaringClassOffset: Int =
      builder.createString(traceElement.getClassName)
    val methodNameOffset = builder.createString(traceElement.getMethodName)
    val fileNameOffset =
      if (traceElement.getFileName != null)
        builder.createString(traceElement.getFileName)
      else 0
    val lineNumber: Int = traceElement.getLineNumber
    StackTraceElement.createStackTraceElement(
      builder,
      declaringClassOffset,
      methodNameOffset,
      fileNameOffset,
      lineNumber
    )
  }
}
