package org.enso.projectmanager.control.core

import zio._

import scala.collection.BuildFrom

/**
  * A traversal over a structure with covariant effects.
  *
  * @tparam F an effectful context
  */
trait Traverse[F[+_, +_]] {

  /**
    * Applies the function `f` to each element of the collection.
    *
    * @param s a collection of elements
    * @param f the mapping function
    * @return the new collection with the function applied to each element
    */
  def traverse[E, A, B, C[+T] <: Iterable[T]](
    s: C[A]
  )(f: A => F[E, B])(implicit bf: BuildFrom[C[A], B, C[B]]): F[E, C[B]]
}

object Traverse {

  def apply[F[+_, +_]](implicit traverse: Traverse[F]): Traverse[F] =
    traverse

  implicit def zioTraverse[R]: Traverse[ZIO[R, +*, +*]] =
    zioTraverseInstance.asInstanceOf[Traverse[ZIO[R, +*, +*]]]

  final private[this] val zioTraverseInstance: Traverse[ZIO[Any, +*, +*]] =
    new ZioTraverse[Any]

}
