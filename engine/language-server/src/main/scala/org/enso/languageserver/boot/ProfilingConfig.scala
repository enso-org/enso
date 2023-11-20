package org.enso.languageserver.boot

import org.apache.commons.io.FilenameUtils

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

/** Application profiling configuration.
  *
  * @param profilingPath the path to the profiling output file
  * @param profilingTime limit the profiling duration, as an infinite profiling
  * duration may cause out-of-memory errors.
  */
case class ProfilingConfig(
  profilingPath: Option[Path]           = None,
  profilingTime: Option[FiniteDuration] = None
) {

  /** Creates the path to the runtime events log with the same name as
    * `profilingPath` but with the `.log` extension.
    *
    * @return the path to the runtime events log file
    */
  def profilingEventsLogPath: Option[Path] =
    profilingPath.map { path =>
      val profilingDirectory     = path.getParent
      val profilingFileName      = path.getFileName.toString
      val profilingFileExtension = FilenameUtils.getExtension(profilingFileName)
      val eventsLogFileName =
        profilingFileName.stripSuffix(
          profilingFileExtension
        ) + ProfilingConfig.EventsLogExtension

      profilingDirectory.resolve(eventsLogFileName)
    }
}
object ProfilingConfig {

  private val EventsLogExtension = "log"
}
