package org.enso.projectmanager.control.core

import zio.ZIO

import scala.collection.BuildFrom

/**
  * Instance of [[Traverse]] class for ZIO.
  */
private[core] class ZioTraverse[R] extends Traverse[ZIO[R, +*, +*]] {

  /** @inheritdoc */
  override def traverse[E, A, B, C[+T] <: Iterable[T]](s: C[A])(
    f: A => ZIO[R, E, B]
  )(implicit bf: BuildFrom[C[A], B, C[B]]): ZIO[R, E, C[B]] =
    ZIO.foreach(s)(f)(bf)

}
