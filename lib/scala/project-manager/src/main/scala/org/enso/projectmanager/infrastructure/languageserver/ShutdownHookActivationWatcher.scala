package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivator.ArePendingShutdownHooks
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivationWatcher.{
  AllShutdownHooksFired,
  WakeUp,
  Watch
}
import org.enso.projectmanager.util.UnhandledLogging
import scala.concurrent.duration._

/** An actor that waits until all shutdown hooks will be fired.
  *
  * @param shutdownHookActivator a reference to an activator
  */
class ShutdownHookActivationWatcher(shutdownHookActivator: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = { case Watch =>
    shutdownHookActivator ! ArePendingShutdownHooks
    context.become(waitingForReply(sender()))
  }

  private def waitingForReply(client: ActorRef): Receive = {
    case true =>
      context.system.scheduler.scheduleOnce(500.millis, self, WakeUp)
      context.become(sleeping(client))

    case false =>
      client ! AllShutdownHooksFired
      context.stop(self)
  }

  private def sleeping(client: ActorRef): Receive = { case WakeUp =>
    shutdownHookActivator ! ArePendingShutdownHooks
    context.become(waitingForReply(client))
  }

}

object ShutdownHookActivationWatcher {

  /** A command that starts watching for a completion of shutdown hooks.
    */
  case object Watch

  /** Signals that all shutdown hooks are completed.
    */
  case object AllShutdownHooksFired

  private case object WakeUp

  /** Creates a configuration object used to create a
    * [[ShutdownHookActivationWatcher]].
    *
    * @param shutdownHookActivator a reference to an activator
    * @return a configuration object
    */
  def props(shutdownHookActivator: ActorRef): Props =
    Props(new ShutdownHookActivationWatcher(shutdownHookActivator))

}
