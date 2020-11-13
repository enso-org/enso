package org.enso.interpreter.instrument.execution

/**
  * Represents completion of computations.
  */
sealed trait Completion

object Completion {

  /**
    * Signals completion of computations.
    */
  case object Done extends Completion

  /**
    * Signals that computations were interrupted.
    */
  case object Interrupted extends Completion

}
