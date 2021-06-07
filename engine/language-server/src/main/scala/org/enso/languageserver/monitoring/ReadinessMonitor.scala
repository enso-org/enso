package org.enso.languageserver.monitoring

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.event.InitializedEvent.{
  InitializationFailed,
  InitializationFinished
}
import org.enso.languageserver.monitoring.MonitoringProtocol.{IsReady, KO, OK}

/** An actor that monitors if the system is ready to accept requests. */
class ReadinessMonitor() extends Actor with LazyLogging {

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, InitializationFinished.getClass)
    context.system.eventStream
      .subscribe(self, InitializationFailed.getClass)
  }

  override def receive: Receive = notReady() orElse stateTransition()

  private def notReady(): Receive = { case IsReady => sender() ! KO }

  private def ready(): Receive = { case IsReady => sender() ! OK }

  private def stateTransition(): Receive = {
    case InitializationFinished =>
      context.become(ready())

    case InitializationFailed =>
      logger.error("Initialization failed. Terminating JVM...")
      System.exit(1)
  }

}

object ReadinessMonitor {

  /** Creates a configuration object used to create a
    * [[ReadinessMonitor]]
    *
    * @return a configuration object
    */
  def props(): Props = Props(new ReadinessMonitor())

}
