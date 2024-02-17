package org.enso.runtimeversionmanager.runner

import com.github.zafarkhaja.semver.Version

/** Represents settings that are used to launch the runner JAR.
  *
  * @param engineVersion Enso engine version to use
  * @param runnerArguments arguments that should be passed to the runner
  * @param connectLoggerIfAvailable specifies if the ran component should
  *                                 connect to launcher's logging service
  */
case class RunSettings(
  engineVersion: Version,
  runnerArguments: Seq[String],
  connectLoggerIfAvailable: Boolean
)
