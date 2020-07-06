package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorLogging, Props}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.event.ProjectEvent.ProjectClosed
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivator.{
  IsEmpty,
  RegisterShutdownHook
}
import org.enso.projectmanager.infrastructure.shutdown.ShutdownHook
import org.enso.projectmanager.util.UnhandledLogging

class ShutdownHookActivator[F[+_, +_]: Exec: CovariantFlatMap]
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ProjectClosed])
  }

  override def receive: Receive = running()

  private def running(
    hooks: Map[UUID, List[ShutdownHook[F]]] =
      Map.empty.withDefaultValue(List.empty)
  ): Receive = {
    case RegisterShutdownHook(projectId, hook) =>
      val realHook = hook.asInstanceOf[ShutdownHook[F]]
      val updated  = hooks.updated(projectId, realHook :: hooks(projectId))
      context.become(running(updated))

    case ProjectClosed(projectId) =>
      val projectHooks = hooks(projectId)
      if (projectHooks.nonEmpty) {
        context.actorOf(
          ShutdownHookRunner.props[F](projectId, projectHooks.reverse)
        )
        context.become(running(hooks - projectId))
      }

    case IsEmpty =>
      val isEmpty = hooks.values.map(_.size).sum == 0
      sender() ! isEmpty
  }

}

object ShutdownHookActivator {

  case class RegisterShutdownHook[F[+_, +_]](
    projectId: UUID,
    hook: ShutdownHook[F]
  )

  case object IsEmpty

  def props[F[+_, +_]: Exec: CovariantFlatMap](): Props =
    Props(new ShutdownHookActivator[F])

}
