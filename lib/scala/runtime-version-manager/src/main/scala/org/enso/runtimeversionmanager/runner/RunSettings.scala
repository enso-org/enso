package org.enso.runtimeversionmanager.runner

import org.enso.semver.SemVer

import java.nio.file.Path

/** Represents settings that are used to launch the runner JAR.
  *
  * @param engineVersion Enso engine version to use
  * @param jvm use JVM - default or provided
  * @param runnerArguments arguments that should be passed to the runner
  * @param workingDirectory the working directory override
  * @param connectLoggerIfAvailable specifies if the ran component should
  * connect to launcher's logging service
  * @param extraEnv extra environment variables
  */
case class RunSettings(
  engineVersion: SemVer,
  jvm: Option[Option[Path]],
  runnerArguments: Seq[String],
  workingDirectory: Option[Path],
  connectLoggerIfAvailable: Boolean,
  extraEnv: Seq[(String, String)] = Seq()
)
