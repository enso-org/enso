package org.enso.projectmanager.infrastructure.shutdown

import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Semaphore, Sync}
import org.enso.projectmanager.infrastructure.log.Logging

/**
  * It enables registering and executing shutdown hooks when the application
  * is shutting down.
  */
class ShutdownHookProcessor[F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap](
  log: Logging[F]
) {

  private var hooks: List[ShutdownHook[F]] = List.empty

  private val semaphore = Semaphore.unsafeMake[F](1)

  /**
    * Registers a shutdown hook.
    *
    * @param shutdownHook the shutdown hook to register
    */
  def registerShutdownHook(shutdownHook: ShutdownHook[F]): F[Nothing, Unit] =
    semaphore.withPermit {
      Sync[F].effect {
        hooks = shutdownHook :: hooks
      }
    }

  /**
    * Fires all registered shutdown hooks.
    */
  def fireShutdownHooks(): F[Nothing, Unit] = {
    def go(): F[Nothing, Unit] =
      for {
        _ <- log.info("Firing all shutdown hooks")
        _ <- traverse(hooks) { _.execute() }
        _ <- log.info("All shutdown hooks fired")
      } yield ()

    if (hooks.nonEmpty) semaphore.withPermit { go() }
    else CovariantFlatMap[F].pure(())
  }

  private def traverse[A, B](
    fa: List[A]
  )(f: A => F[Nothing, B]): F[Nothing, List[B]] =
    fa.foldLeft(CovariantFlatMap[F].pure(List.empty[B])) {
      case (tail, hook) => map2(f(hook), tail)(_ :: _)
    }

  private def map2[A, B, C](fa: F[Nothing, A], fb: F[Nothing, B])(
    f: (A, B) => C
  ): F[Nothing, C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)

}
