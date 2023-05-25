package org.enso.interpreter.test

import org.graalvm.polyglot.PolyglotException

case class InterpreterException(
  @transient polyglotException: PolyglotException,
  @transient output: Option[Any]
) extends Throwable
    with Serializable {
  override def getMessage: String =
    polyglotException.getMessage + output.map("\n" + _.toString()).getOrElse("")
  override def getStackTrace: Array[StackTraceElement] =
    polyglotException.getStackTrace
  override def fillInStackTrace(): Throwable = this
}

object InterpreterException {
  def rethrowPolyglot[T](compute: => T, output: Option[Any] = None): T =
    try {
      compute
    } catch {
      case e: PolyglotException => throw InterpreterException(e, output)
    }

  implicit def toPolyglotException(
    interpreterException: InterpreterException
  ): PolyglotException = interpreterException.polyglotException
}
