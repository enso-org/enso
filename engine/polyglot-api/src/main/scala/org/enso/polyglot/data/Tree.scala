package org.enso.polyglot.data
import com.fasterxml.jackson.annotation.{JsonIgnore, JsonSubTypes, JsonTypeInfo}

import scala.collection.mutable

/** A rose-tree like data structure that distinguishes between root and node
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
      value = classOf[Tree.Node[_]],
      name  = "treeNode"
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

  /** Check whether the tree is not empty. */
  @JsonIgnore
  final def nonEmpty: Boolean =
    !isEmpty

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
    children: Vector[Node[A]]
  ) extends Tree[A]

  case class Node[+A](
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    element: A,
    @JsonTypeInfo(
      use     = JsonTypeInfo.Id.CLASS,
      include = JsonTypeInfo.As.PROPERTY
    )
    children: Vector[Node[A]]
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
      mutable.Builder[Tree.Node[These[A, B]], Vector[Tree.Node[These[A, B]]]]
    def zipImpl(
      res: TreeBuilder,
      t1: mutable.Queue[Tree.Node[A]],
      t2: mutable.Queue[Tree.Node[B]]
    ): Vector[Tree.Node[These[A, B]]] = {
      if (t2.isEmpty) {
        res ++= t1.map(mapNode(_)(These.Here(_)))
        res.result()
      } else if (t1.isEmpty) {
        res ++= t2.map(mapNode(_)(These.There(_)))
        res.result()
      } else {
        val l1 = t1.dequeue()
        t2.dequeueFirst(l => p(l1.element, l.element)) match {
          case Some(l2) =>
            val subforest = zipImpl(
              Vector.newBuilder,
              mutable.Queue(l1.children: _*),
              mutable.Queue(l2.children: _*)
            )
            zipImpl(
              res += Node(These.Both(l1.element, l2.element), subforest),
              t1,
              t2
            )
          case None =>
            zipImpl(res += mapNode(l1)(These.Here(_)), t1, t2)
        }
      }
    }

    ((t1, t2): @unchecked) match {
      case (r1: Root[A] @unchecked, r2: Root[B] @unchecked) =>
        Root(
          zipImpl(
            Vector.newBuilder,
            mutable.Queue(r1.children: _*),
            mutable.Queue(r2.children: _*)
          )
        )
      case (l1: Node[A] @unchecked, l2: Node[B] @unchecked) =>
        Root(zipImpl(Vector.newBuilder, mutable.Queue(l1), mutable.Queue(l2)))
    }
  }

  private def isEmpty[A](tree: Tree[A]): Boolean =
    tree match {
      case Root(children)    => children.isEmpty
      case Node(_, children) => children.isEmpty
    }

  private def mapNode[A, B](tree: Node[A])(f: A => B): Node[B] =
    Node(f(tree.element), tree.children.map(mapNode(_)(f)))

  private def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Root(cx) =>
        Root(cx.map(mapNode(_)(f)))
      case Node(a, cx) =>
        Node(f(a), cx.map(mapNode(_)(f)))
    }

  private def filter[A](tree: Tree[A])(p: A => Boolean): Tree[A] = {
    def filterNode(node: Tree.Node[A]): Option[Tree.Node[A]] = {
      val childrenFiltered = node.children.flatMap(filterNode)
      if (p(node.element)) {
        Some(node.copy(children = childrenFiltered))
      } else if (childrenFiltered.nonEmpty)
        Some(node.copy(children = childrenFiltered))
      else {
        None
      }
    }

    tree match {
      case Root(cx) =>
        Root(cx.flatMap(filterNode))
      case l @ Node(_, _) =>
        Root(filterNode(l).toVector)
    }
  }

  private def fold[A, B](tree: Tree[A], acc: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(acc: B, queue: mutable.Queue[Node[A]]): B =
      if (queue.isEmpty) {
        acc
      } else {
        val node = queue.dequeue()
        queue.enqueueAll(node.children)
        go(f(acc, node.element), queue)
      }

    tree match {
      case Root(children) =>
        go(acc, mutable.Queue(children: _*))
      case Node(element, children) =>
        go(f(acc, element), mutable.Queue(children: _*))
    }
  }

  private def toVector[A](tree: Tree[A]): Vector[A] = {
    val b = Vector.newBuilder[A]
    fold(tree, b)(_ += _)
    b.result()
  }
}
