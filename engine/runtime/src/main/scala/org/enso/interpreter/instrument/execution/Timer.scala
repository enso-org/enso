package org.enso.interpreter.instrument.execution

/** A very simple timer. */
trait Timer {
  def getTime(): Long;
}
object Timer {

  /** A nanosecond precision timer. */
  class Nanosecond extends Timer {
    override def getTime(): Long = System.nanoTime()
  }
}
