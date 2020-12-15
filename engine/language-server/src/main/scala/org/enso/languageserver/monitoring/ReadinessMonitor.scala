package org.enso.languageserver.monitoring

import akka.actor.{Actor, ActorLogging, Props}
import org.enso.languageserver.event.InitializedEvent.{
  InitializationFailed,
  InitializationFinished
}
import org.enso.languageserver.monitoring.MonitoringProtocol.{IsHealthy, KO, OK}

class ReadinessMonitor() extends Actor with ActorLogging {

  override def preStart(): Unit = {
    println("readiness monitor created")
    context.system.eventStream
      .subscribe(self, InitializationFinished.getClass)
    context.system.eventStream
      .subscribe(self, InitializationFailed.getClass)
  }

  override def receive: Receive = notReady() orElse stateTransition()

  private def notReady(): Receive = { case IsHealthy => sender() ! KO }

  private def ready(): Receive = { case IsHealthy => sender() ! OK }

  private def stateTransition(): Receive = {
    case InitializationFinished =>
      println("Initialized")
      context.become(ready())

    case InitializationFailed =>
      log.error("Initialization failed. Terminating JVM")
      System.exit(1)
  }

}

object ReadinessMonitor {

  def props(): Props = Props(new ReadinessMonitor())

}
