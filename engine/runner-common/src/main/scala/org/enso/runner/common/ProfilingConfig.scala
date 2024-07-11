package org.enso.runner.common

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
    profilingPath.map(
      ProfilingConfig.modifyPath(
        _,
        ProfilingConfig.EventsLogSuffix,
        ProfilingConfig.EventsLogExtension
      )
    )
}

object ProfilingConfig {

  private val EventsLogSuffix    = ""
  private val EventsLogExtension = "log"

  /** Modify the path by adding a suffix and changing the file extension.
    *
    * @param path the path to modify
    * @param suffix the suffix to add
    * @param extension the new file extension
    * @return the modified path
    */
  private def modifyPath(
    path: Path,
    suffix: String,
    extension: String
  ): Path = {
    val directory     = path.getParent
    val fileName      = path.getFileName.toString
    val fileExtension = FilenameUtils.getExtension(fileName)
    val modifiedFileName =
      if (fileExtension.isEmpty) {
        s"$fileName$suffix.$extension"
      } else {
        val fileNameWithoutExtension = fileName.stripSuffix(s".$fileExtension")
        s"$fileNameWithoutExtension$suffix.$extension"
      }

    directory.resolve(modifiedFileName)
  }

}
