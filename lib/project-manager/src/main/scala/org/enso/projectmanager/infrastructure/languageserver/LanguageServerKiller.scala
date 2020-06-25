package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  Cancellable,
  PoisonPill,
  Props,
  Terminated
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.ShutDownServer
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerKiller.KillTimeout
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  AllServersKilled,
  KillThemAll
}
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

class LanguageServerKiller(
  controllers: List[ActorRef],
  shutdownTimeout: FiniteDuration
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = {
    case KillThemAll =>
      if (controllers.isEmpty) {
        context.stop(self)
      } else {
        log.info("Killing all servers")
        controllers.foreach(context.watch)
        controllers.foreach(_ ! ShutDownServer)
        val cancellable =
          context.system.scheduler.scheduleOnce(
            shutdownTimeout,
            self,
            KillTimeout
          )
        context.become(killing(controllers.toSet, cancellable, sender()))
      }
  }

  private def killing(
    liveControllers: Set[ActorRef],
    cancellable: Cancellable,
    replyTo: ActorRef
  ): Receive = {
    case Terminated(dead) =>
      val updated = liveControllers - dead
      if (updated.isEmpty) {
        log.info("All language servers have been killed")
        cancellable.cancel()
        replyTo ! AllServersKilled
        context.stop(self)
      } else {
        context.become(killing(updated, cancellable, replyTo))
      }

    case KillTimeout =>
      liveControllers.foreach(_ ! PoisonPill)
      context.stop(self)
  }

}

object LanguageServerKiller {

  case object KillTimeout

  def props(
    controllers: List[ActorRef],
    shutdownTimeout: FiniteDuration
  ): Props = Props(new LanguageServerKiller(controllers, shutdownTimeout))

}
