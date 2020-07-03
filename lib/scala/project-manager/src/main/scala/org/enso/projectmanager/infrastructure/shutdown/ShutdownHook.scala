package org.enso.projectmanager.infrastructure.shutdown

/**
  * Application-level shutdown hook. It allows to run functions before the
  * applications completes its shutdown.
  */
trait ShutdownHook[F[+_, +_]] {

  /**
    * Executes this hook.
    */
  def execute(): F[Nothing, Unit]

}
