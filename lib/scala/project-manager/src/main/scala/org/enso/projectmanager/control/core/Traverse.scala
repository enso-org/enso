package org.enso.projectmanager.control.core

/** A traversal over a structure with effects.
  */
trait Traverse[G[_]] {

  /** Applies the function `f` to each element of the collection.
    *
    * @param s a collection of elements
    * @param f the mapping function
    * @return the new collection with the function applied to each element
    */
  def traverse[F[+_, +_]: Applicative, E, A, B](s: G[A])(
    f: A => F[E, B]
  ): F[E, G[B]]
}

object Traverse {

  def apply[G[_]](implicit traverse: Traverse[G]): Traverse[G] =
    traverse

  implicit object ListTraverse extends Traverse[List] {

    /** @inheritdoc */
    override def traverse[F[+_, +_]: Applicative, E, A, B](
      xs: List[A]
    )(f: A => F[E, B]): F[E, List[B]] =
      xs.foldRight(Applicative[F].pure[E, List[B]](List())) { (a, facc) =>
        Applicative[F].map2(f(a), facc)(_ :: _)
      }
  }

}
