package org.enso.runtimeversionmanager.runner

import org.enso.semver.SemVer

/** Represents settings that are used to launch the runner JAR.
  *
  * @param engineVersion Enso engine version to use
  * @param runnerArguments arguments that should be passed to the runner
  * @param connectLoggerIfAvailable specifies if the ran component should
  *                                 connect to launcher's logging service
  */
case class RunSettings(
  engineVersion: SemVer,
  runnerArguments: Seq[String],
  connectLoggerIfAvailable: Boolean
)
