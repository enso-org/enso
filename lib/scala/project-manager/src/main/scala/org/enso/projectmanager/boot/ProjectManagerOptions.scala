package org.enso.projectmanager.boot

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

/** The runtime options.
  *
  * @param profilingPath the path to the profiling output file
  * @param profilingTime the time limiting the profiling duration
  * @param jvm use JVM - default or with specific path
  */
case class ProjectManagerOptions(
  profilingPath: Option[Path],
  profilingTime: Option[FiniteDuration],
  jvm: Option[Option[Path]]
)
