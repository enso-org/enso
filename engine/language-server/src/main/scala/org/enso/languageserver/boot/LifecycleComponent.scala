package org.enso.languageserver.boot

import org.enso.languageserver.boot.LifecycleComponent.{
  ComponentRestarted,
  ComponentStarted,
  ComponentStopped
}

import scala.concurrent.Future

/**
  * An abstraction for components that can be started and stopped.
  */
trait LifecycleComponent {

  /**
    * Starts asynchronously a server.
    *
    * @return a notice that the server started successfully
    */
  def start(): Future[ComponentStarted.type]

  /**
    * Stops asynchronously a server.
    *
    * @return a notice that the server stopped successfully
    */
  def stop(): Future[ComponentStopped.type]

  /**
    * Restarts asynchronously a server.
    *
    * @return a notice that the server restarted successfully
    */
  def restart(): Future[ComponentRestarted.type]

}

object LifecycleComponent {

  /**
    * Signals that component was started.
    */
  case object ComponentStarted

  /**
    * Signals that component was stopped.
    */
  case object ComponentStopped

  /**
    * Signals that component was restarted.
    */
  case object ComponentRestarted

}
