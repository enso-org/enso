package org.enso.projectmanager.boot

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

/** The runtime options.
  *
  * @param profilingRuntimeEventsLog the path to the runtime events log file
  * @param profilingPath the path to the profiling output file
  * @param profilingTime the time limiting the profiling duration
  */
case class ProjectManagerOptions(
  profilingRuntimeEventsLog: Option[Path],
  profilingPath: Option[Path],
  profilingTime: Option[FiniteDuration]
)
