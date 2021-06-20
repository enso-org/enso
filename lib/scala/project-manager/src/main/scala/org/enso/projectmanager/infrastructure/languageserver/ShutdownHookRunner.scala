package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{Actor, Props, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookRunner.Run
import org.enso.projectmanager.infrastructure.shutdown.ShutdownHook
import org.enso.projectmanager.util.UnhandledLogging

/** An actor that invokes a shutdown hook.
  *
  * @param projectId a project id for which a hook is invoked
  * @param hooks a hook to invoke
  */
class ShutdownHookRunner[F[+_, +_]: Exec: CovariantFlatMap](
  projectId: UUID,
  hooks: List[ShutdownHook[F]]
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    self ! Run
  }

  override def receive: Receive = { case Run =>
    logger.info("Firing shutdown hooks for project [{}].", projectId)
    Exec[F].exec { traverse(hooks) { _.execute() } } pipeTo self
    context.become(running)
  }

  private def running: Receive = {
    case Status.Failure(th) =>
      logger.error(
        s"An error occurred during running shutdown hooks for project [$projectId].",
        th
      )
      context.stop(self)

    case Right(_) =>
      logger.info("All shutdown hooks fired for project [{}].", projectId)
      context.stop(self)
  }

  private def traverse[A, B](
    fa: List[A]
  )(f: A => F[Nothing, B]): F[Nothing, List[B]] =
    fa.foldLeft(CovariantFlatMap[F].pure(List.empty[B])) { case (tail, hook) =>
      map2(f(hook), tail)(_ :: _)
    }

  private def map2[A, B, C](fa: F[Nothing, A], fb: F[Nothing, B])(
    f: (A, B) => C
  ): F[Nothing, C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)

}

object ShutdownHookRunner {

  private case object Run

  /** Creates a configuration object used to create a [[ShutdownHookRunner]].
    *
    * @param projectId a project id for which a hook is invoked
    * @param hooks a hook to invoke
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    projectId: UUID,
    hooks: List[ShutdownHook[F]]
  ): Props = Props(new ShutdownHookRunner[F](projectId, hooks))

}
