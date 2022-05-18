package org.enso.projectmanager.boot

import java.nio.file.Path

/** The runtime options.
  *
  * @param profilingPath the path to the profiling output file.
  */
case class ProjectManagerOptions(profilingPath: Option[Path])
