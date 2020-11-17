package org.enso.projectmanager.infrastructure.languageserver

import scala.concurrent.Future

// TODO [RW] rename it, remove old one
trait LifecycleComponent2 {

  /** Starts asynchronously the server. */
  def start(): Future[Unit]

  /** Stops asynchronously the server. */
  def stop(): Future[Int]

  /** Restarts asynchronously the server. */
  def restart(): Future[Unit]
}
