package org.enso.projectmanager.boot

import zio.{Has, ZEnv}
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.system.System

/**
  * Constants manager for app constants.
  */
object Globals {

  val FailureExitCode = 1

  val SuccessExitCode = 0

  val ConfigFilename = "application.conf"

  val ConfigNamespace = "project-manager"

  val zioEnvironment: ZEnv =
    Has.allOf[
      Clock.Service,
      Console.Service,
      System.Service,
      Random.Service,
      Blocking.Service
    ](
      Clock.Service.live,
      Console.Service.live,
      System.Service.live,
      Random.Service.live,
      Blocking.Service.live
    )

}
