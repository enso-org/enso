package org.enso.interpreter.test

import org.graalvm.polyglot.PolyglotException

case class InterpreterException(
  @transient polyglotException: PolyglotException
) extends Throwable
    with Serializable {
  override def getMessage: String = polyglotException.getMessage
  override def getStackTrace: Array[StackTraceElement] =
    polyglotException.getStackTrace
  override def fillInStackTrace(): Throwable = this
}

object InterpreterException {
  def rethrowPolyglot[T](compute: => T): T =
    try {
      compute
    } catch { case e: PolyglotException => throw InterpreterException(e) }

  implicit def toPolyglotException(
    interpreterException: InterpreterException
  ): PolyglotException = interpreterException.polyglotException

}
