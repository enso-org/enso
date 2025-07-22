package org.enso.projectmanager.boot.configuration

import org.slf4j.event.Level

import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration

/** The options supplied (e.g. with the command line options when starting the
  * main project manager process.
  *
  * @param logLevel the logging level
  * @param profilingPath the path to the profiling out file
  * @param profilingTime the time limiting the profiling duration
  * @param jvm enable JVM mode with default or provided JVM
  * @param extraEnv extra environment variables
  */
case class MainProcessConfig(
  logLevel: Level,
  profilingPath: Option[Path],
  profilingTime: Option[FiniteDuration],
  jvm: Option[Option[Path]],
  extraEnv: Seq[(String, String)]
)
