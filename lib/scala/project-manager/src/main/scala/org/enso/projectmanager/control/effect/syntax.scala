package org.enso.projectmanager.control.effect

object syntax {

  /** Implicit conversion to [[ErrorChannelOps]]
    *
    * @param fa an effect
    * @return
    */
  implicit def toSyncOps[F[+_, +_]: Sync, E, A](fa: F[E, A]): SyncOps[F, E, A] =
    new SyncOps[F, E, A](fa)

  /** Implicit conversion to [[ErrorChannelOps]]
    *
    * @param fa an effect
    * @return
    */
  implicit def toErrorChannelOps[F[+_, +_]: ErrorChannel, E, A](
    fa: F[E, A]
  ): ErrorChannelOps[F, E, A] =
    new ErrorChannelOps[F, E, A](fa)

}
