package org.enso.polyglot.data
import com.fasterxml.jackson.annotation.{JsonIgnore, JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable

/** A rose-tree like data structure that distinguishes between root and leaf
  * elements.
  */
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

  /** Build a new tree by applying a function to all elements of this tree.
    *
    * @param f the function to apply to each element
    * @return the new tree after applying the function `f` to elements
    */
  @JsonIgnore
  final def map[B](f: A => B): Tree[B] =
    Tree.map(this)(f)

  /** Selects all elements which satisfy a predicate.
    *
    * @param p the predicate used to test elements
    * @return a new tree consisting of all elements of this tree that satisfy
    * the given predicate p.
    */
  @JsonIgnore
  final def filter(p: A => Boolean): Tree[A] =
    Tree.filter(this)(p)

  /** Fold the elements using the specified associative binary operator.
    *
    * @param acc the neutral element for the fold operation
    * @param f a binary operator
    * @return the result of applying the fold operator f between all the
    * elements and `acc`
    */
  @JsonIgnore
  final def fold[B](acc: B)(f: (B, A) => B): B =
    Tree.fold(this, acc)(f)

  /** Join this and that trees using the equality function.
    *
    * @param that the tree to join with
    * @return the result of joining this and that trees
    */
  @JsonIgnore
  final def zip[B](that: Tree[B]): Tree[These[A, B]] =
    Tree.zip(this, that)

  /** Join this and that trees using the binary function p to tests the
    * matching element.
    *
    * For each node on the corresponding tree level:
    * - return `Here` if the node presents in this tree
    * - return `There` if the node presents in that tree
    * - return `Both` if the node presents in both trees according to the
    *   predicate p
    *
    * @param that the tree to join with
    * @param p the predicate comparing the elements
    * @return the result of joining this and that trees
    */
  @JsonIgnore
  final def zipBy[B](that: Tree[B])(p: (A, B) => Boolean): Tree[These[A, B]] =
    Tree.zipBy(this, that)(p)

  /** Check whether the tree is empty. */
  @JsonIgnore
  final def isEmpty: Boolean =
    Tree.isEmpty(this)

  /** Convert tree to vector. */
  @JsonIgnore
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

  /** An empty tree. */
  def empty[A]: Tree[A] = Root(Vector())

  /** Join this and that trees using the equality function.
    *
    * @param t1 the first tree to join
    * @param t2 the second tree to join
    * @return the result of joining two trees
    */
  def zip[A, B, T[_] <: Tree[_]](t1: T[A], t2: T[B]): Tree[These[A, B]] =
    zipBy(t1, t2)(_ == _)

  /** Join two trees using the binary function p to tests the matching element.
    *
    * For each node on the corresponding tree level:
    * - return `Here` if the node presents in the first tree
    * - return `There` if the node presents in the second tree
    * - return `Both` if the node presents in both trees according to the
    *   predicate p
    *
    * @param t1 the first tree to join
    * @param t2 the second tree to join
    * @param p the predicate comparing the elements
    * @return the result of joining two trees
    */
  def zipBy[A, B, T[_] <: Tree[_]](t1: T[A], t2: T[B])(
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

    ((t1, t2): @unchecked) match {
      case (r1: Root[A] @unchecked, r2: Root[B] @unchecked) =>
        Root(
          go(
            Vector.newBuilder,
            mutable.Queue(r1.children: _*),
            mutable.Queue(r2.children: _*)
          )
        )
      case (l1: Leaf[A] @unchecked, l2: Leaf[B] @unchecked) =>
        Root(go(Vector.newBuilder, mutable.Queue(l1), mutable.Queue(l2)))
    }
  }

  private def isEmpty[A](tree: Tree[A]): Boolean =
    tree match {
      case Root(children)    => children.isEmpty
      case Leaf(_, children) => children.isEmpty
    }

  private def mapLeaf[A, B](tree: Leaf[A])(f: A => B): Leaf[B] =
    Leaf(f(tree.element), tree.children.map(mapLeaf(_)(f)))

  private def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Root(cx) =>
        Root(cx.map(mapLeaf(_)(f)))
      case Leaf(a, cx) =>
        Leaf(f(a), cx.map(mapLeaf(_)(f)))
    }

  private def filter[A](tree: Tree[A])(p: A => Boolean): Tree[A] = {
    def filterLeaf(leaf: Tree.Leaf[A]): Option[Tree.Leaf[A]] = {
      val childrenFiltered = leaf.children.flatMap(filterLeaf)
      if (p(leaf.element)) {
        Some(leaf.copy(children = childrenFiltered))
      } else if (childrenFiltered.nonEmpty)
        Some(leaf.copy(children = childrenFiltered))
      else {
        None
      }
    }

    tree match {
      case Root(cx) =>
        Root(cx.flatMap(filterLeaf))
      case l @ Leaf(_, _) =>
        Root(filterLeaf(l).toVector)
    }
  }

  private def fold[A, B](tree: Tree[A], acc: B)(f: (B, A) => B): B = {
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

  private def toVector[A](tree: Tree[A]): Vector[A] = {
    val b = Vector.newBuilder[A]
    fold(tree, b)(_ += _)
    b.result()
  }
}
