package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.event.ProjectEvent.ProjectClosed
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivator.{
  ArePendingShutdownHooks,
  RegisterShutdownHook,
  ShutdownHooksFired
}
import org.enso.projectmanager.infrastructure.shutdown.ShutdownHook
import org.enso.projectmanager.util.UnhandledLogging

import java.util.UUID

/** The ShutdownHookActivator is responsible for receiving asynchronously
  * [[ProjectClosed]] events and invoking all shutdown hooks for the closed
  * project.
  */
class ShutdownHookActivator[F[+_, +_]: Exec: CovariantFlatMap]
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ProjectClosed])
  }

  override def receive: Receive = running()

  private def running(
    hooks: Map[UUID, List[ShutdownHook[F]]] =
      Map.empty.withDefaultValue(List.empty),
    scheduled: List[UUID] = Nil
  ): Receive = {
    case RegisterShutdownHook(projectId, hook) =>
      val realHook = hook.asInstanceOf[ShutdownHook[F]]
      val updated  = hooks.updated(projectId, realHook :: hooks(projectId))
      context.become(running(updated, scheduled))

    case ProjectClosed(projectId) =>
      val projectHooks = hooks(projectId)
      if (projectHooks.nonEmpty) {
        context.actorOf(
          ShutdownHookRunner.props[F](projectId, projectHooks.reverse)
        )
        context.become(running(hooks - projectId, projectId :: scheduled))
      } else if (scheduled.contains(projectId)) {
        logger.debug(
          s"Request for starting shutdown hooks has already been filed for project ${projectId}. Ignoring."
        )
      } else {
        logger.warn(
          s"Shutdown hook activator has no recollection of project ${projectId}. Either it was closed already or it never existed. Ignoring."
        )
      }

    case ShutdownHooksFired(projectId) =>
      context.become(running(hooks, scheduled.filter(_ != projectId)))

    case ArePendingShutdownHooks =>
      val arePending = hooks.values.map(_.size).sum != 0 || scheduled.nonEmpty
      sender() ! arePending
  }

}

object ShutdownHookActivator {

  /** A command used to register a project shutdown hook. */
  sealed case class RegisterShutdownHook[F[+_, +_]](
    projectId: UUID,
    hook: ShutdownHook[F]
  )

  /** Requests an activator if there are pending shutdown hooks. */
  case object ArePendingShutdownHooks

  /** Notifies that all shutdown hooks are fired for the project. */
  sealed case class ShutdownHooksFired(projectId: UUID)

  /** Creates a configuration object used to create a [[ShutdownHookActivator]].
    *
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](): Props =
    Props(new ShutdownHookActivator[F])

}
