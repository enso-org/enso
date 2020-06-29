package org.enso.searcher

trait Database[F[_], G[_]] {

  def run[A](program: F[A]): G[A]

  def close(): Unit
}
