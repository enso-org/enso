package org.enso.interpreter.instrument.execution

/** A very simple timer. */
trait Timer {
  def getTime(): Long;
}
object Timer {

  /** A nanosecond precision timer. */
  final class Nanosecond extends Timer {
    override def getTime(): Long = System.nanoTime()
  }

  /** A timer returns the same zero time. */
  final class Disabled extends Timer {
    override def getTime(): Long = 0
  }
}
