package org.enso.launcher.components.runner

import nl.gn0s1s.bump.SemVer

/** Represents settings that are used to launch the runner JAR.
  *
  * @param version Enso engine version to use
  * @param runnerArguments arguments that should be passed to the runner
  * @param connectLoggerIfAvailable specifies if the ran component should
  *                                 connect to launcher's logging service
  */
case class RunSettings(
  version: SemVer,
  runnerArguments: Seq[String],
  connectLoggerIfAvailable: Boolean
)
