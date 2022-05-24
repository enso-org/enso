package org.enso.languageserver.boot

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

/** Application profiling configuration.
  *
  * @param runtimeEventsLogPath the path to the runtime events log file
  * @param profilingPath the path to the profiling output file
  * @param profilingTime limit the profiling duration, as an infinite profiling
  * duration may cause out-of-memory errors.
  */
case class ProfilingConfig(
  runtimeEventsLogPath: Option[Path]    = None,
  profilingPath: Option[Path]           = None,
  profilingTime: Option[FiniteDuration] = None
)
