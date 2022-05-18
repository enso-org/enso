package org.enso.languageserver.boot

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

/** Application profiling configuration.
  *
  * @param profilingPath the path to the profiling output file
  * @param profilingTime limit the profiling duration, as an infinite profiling
  * duration may cause out-of-memory errors.
  */
case class ProfilingConfig(
  profilingPath: Option[Path],
  profilingTime: Option[FiniteDuration]
)
