package org.enso.interpreter.test

import org.graalvm.polyglot.{PolyglotException, Value}

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
  def rethrowPolyglot(compute: => Value): Value =
    try {
      compute
    } catch { case e: PolyglotException => throw InterpreterException(e) }
}
