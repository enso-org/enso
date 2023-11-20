package org.enso.languageserver.profiling

object ProfilingProtocol {

  /** A request to start the profiling. */
  case object ProfilingStartRequest

  /** A response to request to start the profiling. */
  case object ProfilingStartResponse

  /** A request to stop the profiling. */
  case object ProfilingStopRequest

  /** A response to request to stop the profiling. */
  case object ProfilingStopResponse

  /** A request to create a memory snapshot. */
  case object ProfilingSnapshotRequest

  /** A response to request to create a memory snapshot. */
  case object ProfilingSnapshotResponse

}
