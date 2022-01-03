package org.enso.projectmanager.boot

import zio.ExitCode

/** Constants manager for app constants. */
object Globals {

  val FailureExitCode: ExitCode = ExitCode(1)

  val SuccessExitCode: ExitCode = ExitCode(0)

  val ConfigFilename = "application.conf"

  val ConfigNamespace = "project-manager"
}
