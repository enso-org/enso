package org.enso.polyglot.data
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[Tree.Root[_]],
      name  = "treeRoot"
    ),
    new JsonSubTypes.Type(
      value = classOf[Tree.Leaf[_]],
      name  = "treeLeaf"
    )
  )
)
sealed trait Tree[+A] {

  final def map[B](f: A => B): Tree[B] =
    Tree.map(this)(f)

  final def fold[B](acc: B)(f: (B, A) => B): B =
    Tree.fold(this, acc)(f)

  final def toVector: Vector[A] =
    Tree.toVector(this)
}

object Tree {

  case class Root[+A](
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    children: Vector[Leaf[A]]
  ) extends Tree[A]

  case class Leaf[+A](
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    element: A,
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    children: Vector[Leaf[A]]
  ) extends Tree[A]

  def fold[A, B](tree: Tree[A], acc: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(acc: B, queue: mutable.Queue[Leaf[A]]): B =
      if (queue.isEmpty) {
        acc
      } else {
        val leaf = queue.dequeue()
        queue.enqueueAll(leaf.children)
        go(f(acc, leaf.element), queue)
      }

    tree match {
      case Root(children) =>
        go(acc, mutable.Queue(children: _*))
      case Leaf(element, children) =>
        go(f(acc, element), mutable.Queue(children: _*))
    }
  }

  def zip[A, B](t1: Tree.Root[A], t2: Tree.Root[B]): Tree[These[A, B]] =
    zipBy(t1, t2)(_ == _)

  def zipBy[A, B](t1: Tree.Root[A], t2: Tree.Root[B])(
    p: (A, B) => Boolean
  ): Tree[These[A, B]] = {
    type TreeBuilder =
      mutable.Builder[Tree.Leaf[These[A, B]], Vector[Tree.Leaf[These[A, B]]]]
    def go(
      res: TreeBuilder,
      t1: mutable.Queue[Tree.Leaf[A]],
      t2: mutable.Queue[Tree.Leaf[B]]
    ): Vector[Tree.Leaf[These[A, B]]] = {
      if (t2.isEmpty) {
        res ++= t1.map(mapLeaf(_)(These.Here(_)))
        res.result()
      } else if (t1.isEmpty) {
        res ++= t2.map(mapLeaf(_)(These.There(_)))
        res.result()
      } else {
        val l1 = t1.dequeue()
        t2.dequeueFirst(l => p(l1.element, l.element)) match {
          case Some(l2) =>
            val subforest = go(
              Vector.newBuilder,
              mutable.Queue(l1.children: _*),
              mutable.Queue(l2.children: _*)
            )
            go(
              res += Leaf(These.Both(l1.element, l2.element), subforest),
              t1,
              t2
            )
          case None =>
            go(res += mapLeaf(l1)(These.Here(_)), t1, t2)
        }
      }
    }

    Root(
      go(
        Vector.newBuilder,
        mutable.Queue(t1.children: _*),
        mutable.Queue(t2.children: _*)
      )
    )
  }

  private def mapLeaf[A, B](tree: Leaf[A])(f: A => B): Leaf[B] =
    Leaf(f(tree.element), tree.children.map(mapLeaf(_)(f)))

  private def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Root(children)    => Root(children.map(mapLeaf(_)(f)))
      case leaf @ Leaf(_, _) => mapLeaf(leaf)(f)
    }

  private def toVector[A](tree: Tree[A]): Vector[A] = {
    val b = Vector.newBuilder[A]
    fold(tree, b)(_ += _)
    b.result()
  }
}
