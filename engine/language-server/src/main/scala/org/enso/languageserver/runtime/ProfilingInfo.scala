package org.enso.languageserver.runtime

/** A trait to unify various types of profiling information about an executed
  * expression.
  */
sealed trait ProfilingInfo
object ProfilingInfo {

  /** The time spent executing the expression.
    *
    * @param nanoTime the time elapsed while executing the expression in
    *                 nanoseconds
    */
  case class ExecutionTime(nanoTime: Long) extends ProfilingInfo
}
