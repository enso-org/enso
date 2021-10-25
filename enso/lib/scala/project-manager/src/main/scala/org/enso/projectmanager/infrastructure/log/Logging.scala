package org.enso.projectmanager.infrastructure.log

/** A pure functional logging facility.
  *
  * @tparam F a monadic context
  */
trait Logging[F[+_, +_]] {

  def debug(msg: String): F[Nothing, Unit]

  def debug(msg: String, args: AnyRef*): F[Nothing, Unit]

  def info(msg: String): F[Nothing, Unit]

  def info(msg: String, args: AnyRef*): F[Nothing, Unit]

  def error(msg: String): F[Nothing, Unit]

  def error(msg: String, args: AnyRef*): F[Nothing, Unit]
}
