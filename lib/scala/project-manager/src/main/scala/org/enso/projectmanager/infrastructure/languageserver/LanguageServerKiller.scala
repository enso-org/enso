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

/**
  * An actor that shuts all running language servers. It orchestrates all
  * language server controllers to shut down gracefully servers. When it is not
  * possible it kills all controller actors.
  *
  * @param controllers running controllers
  * @param shutdownTimeout a shutdown timeout
  */
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
        sender() ! AllServersKilled
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

  private case object KillTimeout

  /**
    * Creates configuration object used to create a [[LanguageServerKiller]].
    *
    * @param controllers running controllers
    * @param shutdownTimeout a shutdown timeout
    * @return a configuration object
    */
  def props(
    controllers: List[ActorRef],
    shutdownTimeout: FiniteDuration
  ): Props = Props(new LanguageServerKiller(controllers, shutdownTimeout))

}
