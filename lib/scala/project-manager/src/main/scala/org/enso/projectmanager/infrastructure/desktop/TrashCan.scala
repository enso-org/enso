package org.enso.projectmanager.infrastructure.desktop

import java.io.File

/** An abstraction form the trash can.
  *
  * @tparam F a monadic context
  */
trait TrashCan[F[+_, +_]] {

  /** Moves the provided path to the trash bin.
    *
    * @return `true` if the operation was successful
    */
  def moveToTrash(path: File): F[Nothing, Boolean]
}
