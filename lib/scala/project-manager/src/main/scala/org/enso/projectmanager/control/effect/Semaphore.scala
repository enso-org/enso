package org.enso.projectmanager.control.effect

import org.enso.projectmanager.control.core.CovariantFlatMap

/** A pure functional semaphore.
  *
  * @tparam F an effectful context
  */
trait Semaphore[F[+_, +_]] {

  /** Acquires one permit from this semaphore, blocking until one is
    * available.
    *
    * @return
    */
  def acquire(): F[Nothing, Unit]

  /** Releases one permit, returning it to the semaphore.
    *
    * @return
    */
  def release(): F[Nothing, Unit]

  /** Executes the critical section acquiring (before) and releasing (after)
    * one permit.
    *
    * @param criticalSection a code accessing shared resource
    * @return
    */
  def withPermit[E, A](criticalSection: F[E, A]): F[E, A]

}

object Semaphore {

  def unsafeMake[F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap](
    permits: Int
  ): Semaphore[F] =
    new JvmSemaphore(permits)

}
