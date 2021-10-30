package org.enso.projectmanager.control.effect

import java.util.concurrent

import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import syntax._

/** Java-backed pure semaphore implementation.
  *
  * @param permits
  * @tparam F a monadic context
  */
class JvmSemaphore[F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap](
  permits: Int
) extends Semaphore[F] {

  private val semaphore = new concurrent.Semaphore(permits, true)

  /** @inheritdoc */
  override def acquire(): F[Nothing, Unit] =
    Sync[F].effect(semaphore.acquire())

  /** @inheritdoc */
  override def release(): F[Nothing, Unit] =
    Sync[F].effect(semaphore.release())

  /** @inheritdoc */
  override def withPermit[E, A](criticalSection: F[E, A]): F[E, A] =
    for {
      _      <- acquire()
      result <- criticalSection.onDie(_ => release()).onError(_ => release())
      _      <- release()
    } yield result

}
