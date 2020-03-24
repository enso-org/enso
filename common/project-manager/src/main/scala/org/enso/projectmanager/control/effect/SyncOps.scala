package org.enso.projectmanager.control.effect

import scala.concurrent.duration.FiniteDuration

class SyncOps[F[+_, +_]: Sync, E, A](fa: F[E, A]) {

  def timeoutFail[E1 >: E](e: E1)(timeout: FiniteDuration): F[E1, A] =
    Sync[F].timeoutFail[E, E1, A](fa)(e)(timeout)

}
