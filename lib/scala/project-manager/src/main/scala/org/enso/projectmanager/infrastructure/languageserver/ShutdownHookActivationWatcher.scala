package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivator.IsEmpty
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivationWatcher.{
  AllShutdownHooksFired,
  WakeUp,
  Watch
}
import org.enso.projectmanager.util.UnhandledLogging
import scala.concurrent.duration._

class ShutdownHookActivationWatcher(shutdownHookActivator: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = {
    case Watch =>
      shutdownHookActivator ! IsEmpty
      context.become(waitingForReply(sender()))
  }

  def waitingForReply(client: ActorRef): Receive = {
    case false =>
      context.system.scheduler.scheduleOnce(500.millis, self, WakeUp)
      context.become(sleeping(client))

    case true =>
      client ! AllShutdownHooksFired
      context.stop(self)
  }

  def sleeping(client: ActorRef): Receive = {
    case WakeUp =>
      shutdownHookActivator ! IsEmpty
      context.become(waitingForReply(client))
  }

}

object ShutdownHookActivationWatcher {

  case object Watch

  case object AllShutdownHooksFired

  private case object WakeUp

  def props(shutdownHookActivator: ActorRef): Props =
    Props(new ShutdownHookActivationWatcher(shutdownHookActivator))

}
