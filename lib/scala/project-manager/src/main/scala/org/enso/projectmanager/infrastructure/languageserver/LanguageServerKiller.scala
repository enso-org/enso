package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, ActorRef, Cancellable, PoisonPill, Props, Terminated}
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.event.ProjectEvent.ProjectClosed
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.ShutDownServer
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerKiller.KillTimeout
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  AllServersKilled,
  KillThemAll
}
import org.enso.projectmanager.util.UnhandledLogging

import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/** An actor that shuts all running language servers. It orchestrates all
  * language server controllers to shut down gracefully servers. When it is not
  * possible it kills all controller actors.
  *
  * @param controllers running controllers
  * @param shutdownTimeout a shutdown timeout
  */
class LanguageServerKiller(
  controllers: Map[UUID, ActorRef],
  shutdownTimeout: FiniteDuration
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = { case KillThemAll =>
    if (controllers.isEmpty) {
      sender() ! AllServersKilled
      context.stop(self)
    } else {
      logger.info("Killing all servers [{}].", controllers)
      controllers.foreach { case (_, ref) =>
        context.watch(ref)
        ref ! ShutDownServer
      }
      val cancellable =
        context.system.scheduler.scheduleOnce(
          shutdownTimeout,
          self,
          KillTimeout
        )
      context.become(killing(controllers.map(_.swap), cancellable, sender()))
    }
  }

  private def killing(
    liveControllers: Map[ActorRef, UUID],
    cancellable: Cancellable,
    replyTo: ActorRef
  ): Receive = {
    case Terminated(dead) =>
      val updated = liveControllers - dead
      if (updated.isEmpty) {
        logger.info("All language servers have been killed.")
        cancellable.cancel()
        replyTo ! AllServersKilled
        context.stop(self)
      } else {
        context.become(killing(updated, cancellable, replyTo))
      }

    case KillTimeout =>
      logger.warn(
        s"Not all language servers' controllers finished on time. Forcing termination."
      )
      liveControllers.foreach { case (actorRef, projectId) =>
        actorRef ! PoisonPill
        context.system.eventStream.publish(ProjectClosed(projectId))
      }
      context.stop(self)
  }

}

object LanguageServerKiller {

  private case object KillTimeout

  /** Creates configuration object used to create a [[LanguageServerKiller]].
    *
    * @param controllers running controllers
    * @param shutdownTimeout a shutdown timeout
    * @return a configuration object
    */
  def props(
    controllers: Map[UUID, ActorRef],
    shutdownTimeout: FiniteDuration
  ): Props = Props(new LanguageServerKiller(controllers, shutdownTimeout))

}
