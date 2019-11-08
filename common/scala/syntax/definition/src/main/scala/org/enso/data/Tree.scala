package org.enso.data

case class Tree[K, V](value: Option[V], branches: Map[K, Tree[K, V]]) {
  def +(item: (List[K], V)): Tree[K, V] = item._1 match {
    case Nil => this.copy(value = Some(item._2))
    case p :: ps => {
      val newBranch = branches.getOrElse(p, Tree[K, V]()) + (ps -> item._2)
      this.copy(branches = branches + (p -> newBranch))
    }
  }

  def map[S](f: V => S): Tree[K, S] =
    Tree(value.map(f), branches.mapValues(_.map(f)))

  def dropValues(): Tree[K, Unit] =
    map(_ => ())

  def get(key: K): Option[Tree[K, V]] =
    branches.get(key)

  def get(path: List[K]): Option[Tree[K, V]] = path match {
    case Nil     => Some(this)
    case p :: ps => branches.get(p).flatMap(_.get(ps))
  }

  def getValue(path: List[K]): Option[V] =
    get(path).flatMap(_.value)

  def isLeaf: Boolean =
    branches.isEmpty
}

object Tree {
  def apply[K, V](): Tree[K, V] = new Tree(None, Map())
  def apply[K, V](deps: (List[K], V)*): Tree[K, V] =
    deps.foldLeft(Tree[K, V]())(_ + _)
}
