package org.enso.languageserver.profiling

object ProfilingProtocol {

  /** A request to start the profiling. */
  case object ProfilingStartRequest

  case object ProfilingStartResponse

  /** A request to stop the profiling. */
  case object ProfilingStopRequest

  case class ProfilingStopResponse(result: Array[Byte])

}
