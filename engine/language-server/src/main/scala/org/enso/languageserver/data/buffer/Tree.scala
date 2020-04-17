package org.enso.languageserver.data.buffer

import cats.kernel.Monoid

/**
  * A super class of nodes stored in the tree.
  *
  * @tparam C the container for leaf elements.
  * @tparam M the monoid used for sizing subtrees.
  */
sealed trait NodeVal[C, M] {

  /**
    * Iterates through all leaves, passing their contents to the specified
    * consumer function.
    *
    * @param f the consumer function.
    */
  def foreach(f: C => Unit): Unit
}

/**
  * An empty leaf of the tree.
  *
  * @tparam C the container for leaf elements.
  * @tparam M the monoid used for sizing subtrees.
  */
case class Empty[C, M]() extends NodeVal[C, M] {
  override def foreach(f: C => Unit): Unit = ()
}

/**
  * An internal node of the tree, storing children nodes.
  *
  * @param children the children nodes.
  * @tparam C the container for leaf elements.
  * @tparam M the monoid used for sizing subtrees.
  */
case class Internal[C, M](
  children: Array[Node[C, M]]
) extends NodeVal[C, M] {
  override def foreach(f: C => Unit): Unit =
    children.foreach(_.value.foreach(f))
}

/**
  * A leaf node, storing a collection of type `C`.
  *
  * @param elements the collection stored in this leaf.
  * @tparam C the container for leaf elements.
  * @tparam M the monoid used for sizing subtrees.
  */
case class Leaf[C, M](
  elements: C
) extends NodeVal[C, M] {
  override def foreach(f: C => Unit): Unit = f(elements)
}

/**
  * A B-Tree node.
  *
  * A node must satisfy the following invariants to be considered valid:
  * 1. All leaves appear at the same depth (i.e. for each node, the height of
  *    all its children must be equal);
  * 2. All `Internal` nodes except for the root have between `minChildren` and
  *    `maxChildren` (both inclusive) children, as specified by the relevant
  *    [[TreeShape]];
  * 3. A root node has at least 2 children.
  *
  * @param height the height of this node.
  * @param measure the "size", as measured by some monoid.
  * @param value the [[NodeVal]] stored in this node.
  * @tparam C the type of containers stored in the leaves.
  * @tparam M the type of measuring monoid.
  */
case class Node[C, M](
  height: Int,
  measure: M,
  value: NodeVal[C, M]
) {
  private def unsafeChildren: Array[Node[C, M]] = value match {
    case Internal(children) => children
    case Leaf(_) =>
      throw new Exception("Node.unsafeChildren called on a Leaf.")
    case Empty() =>
      throw new Exception("Node.unsafeChildren called on an Empty node.")
  }

  private def isEmpty: Boolean = value match {
    case Empty() => true
    case _       => false
  }

  private def canBecomeChild(implicit treeShape: TreeShape): Boolean =
    value match {
      case Leaf(_)            => true
      case Internal(children) => children.length >= treeShape.minChildren
      case Empty()            => false
    }

  /**
    * Concatenates two trees, attaching `that` after `this`, ensuring the
    * result is a valid node.
    *
    * @param that the node to attach at the end of `this`.
    * @param treeShape the tree shape specification for this tree.
    * @param measureMonoid the monoid instance for the measure type.
    * @return a tree resulting from concatenating `this` and `that`.
    */
  def ++(
    that: Node[C, M]
  )(implicit treeShape: TreeShape, measureMonoid: Monoid[M]): Node[C, M] = {

    // get rid of empty arguments as soon as possible
    if (isEmpty) return that
    if (that.isEmpty) return this

    if (height < that.height) {
      // `that` has depth of at least 1 here, so we're guaranteed a non-empty
      // children list.
      val thatChildren = that.unsafeChildren
      if (height == that.height - 1 && canBecomeChild) {
        // `this` would be the same height as any of `thatChildren`, so they
        // form a valid children list together.
        Node.unsafeMergeChildren(Array(this), thatChildren)
      } else {
        // Either `this` is inserted lower into the tree, preserving the height
        // of the first child or a new tree was formed, increasing the height
        // by one.
        val newNode           = this ++ thatChildren(0)
        val remainingChildren = thatChildren.drop(1)
        if (newNode.height == that.height - 1) {
          // `newNode` has the same height as any of `thatChildren`, so they
          // can be made children of a new node together.
          Node.unsafeMergeChildren(Array(newNode), remainingChildren)
        } else {
          // all children of `newNode` are the same height as children of
          // `that`. That also means `newNode` definitely has children.
          Node.unsafeMergeChildren(newNode.unsafeChildren, remainingChildren)
        }
      }
    } else if (height == that.height) {
      if (this.canBecomeChild && that.canBecomeChild) {
        // Both nodes have the same height and both are OK as children.
        Node.unsafeFromChildren(Array(this, that))
      } else {
        // Both nodes are Internal, because otherwise they could become
        // children, so they definitely have children.
        Node.unsafeMergeChildren(this.unsafeChildren, that.unsafeChildren)
      }
    } else {
      // This case is exactly symmetric to the first one.
      val thisChildren = unsafeChildren
      if (that.height == height - 1 && that.canBecomeChild) {
        Node.unsafeMergeChildren(thisChildren, Array(that))
      } else {
        val lastChildIdx      = thisChildren.length - 1
        val newNode           = thisChildren(lastChildIdx) ++ that
        val remainingChildren = thisChildren.take(lastChildIdx)
        if (newNode.height == this.height - 1) {
          Node.unsafeMergeChildren(remainingChildren, Array(newNode))
        } else {
          Node.unsafeMergeChildren(remainingChildren, newNode.unsafeChildren)
        }
      }
    }
  }

  /**
    * Constructs a subtree consisting of an initial subset of elements.
    *
    * @param offset the length of the subsequence to construct.
    * @param measureOps operations to interpret this tree's measure.
    * @param measureMonoid the monoid instance for the measure.
    * @param measurable measuring operations for leaves.
    * @param treeShape the shape constraints for this tree.
    * @tparam I the type of indices used for this operation.
    * @return an initial subsequence of size `offset` according to specific
    *         measure interpretation.
    */
  def take[I](
    offset: I,
    measureOps: RangeOps[I, C, M]
  )(
    implicit measureMonoid: Monoid[M],
    measurable: Measurable[C, M],
    treeShape: TreeShape
  ): Node[C, M] = {
    value match {
      case Empty() => this
      case Leaf(c) => Node(measureOps.take(c, offset))

      case Internal(children) =>
        val (left, mid, _) = findIndexChild(offset, measureOps, children)

        mid match {
          case Some((newOffset, mid)) =>
            val tail = mid.take(newOffset, measureOps)
            Node.mergeTrees(left :+ tail)
          case None => Node.mergeTrees(left)
        }
    }
  }

  /**
    * Constructs a subtree by removing the first `offset` elements.
    *
    * @param offset the number of elements to drop.
    * @param measureOps operations to interpret this tree's measure.
    * @param measureMonoid the monoid instance for the measure.
    * @param measurable measuring operations for leaves.
    * @param treeShape the shape constraints for this tree.
    * @tparam I the type of indices used for this operation.
    * @return a subsequence created by dropping `offset` elements, according
    *         to specific measure interpretation.
    */
  def drop[I](
    offset: I,
    measureOps: RangeOps[I, C, M]
  )(
    implicit measureMonoid: Monoid[M],
    measurable: Measurable[C, M],
    treeShape: TreeShape
  ): Node[C, M] = {
    value match {
      case Empty() => this
      case Leaf(c) => Node(measureOps.drop(c, offset))
      case Internal(children) =>
        val (_, mid, right) = findIndexChild(offset, measureOps, children)
        mid match {
          case Some((newOffset, mid)) =>
            val head = mid.drop(newOffset, measureOps)
            Node.mergeTrees(head :: right)
          case None => Node.mergeTrees(right)
        }
    }
  }

  private def findIndexChild[I](
    ix: I,
    measureOps: RangeOps[I, C, M],
    childrenArray: Array[Node[C, M]]
  ): (List[Node[C, M]], Option[(I, Node[C, M])], List[Node[C, M]]) = {
    val children  = childrenArray.toList
    var currentIx = ix
    val leftChildren = children.takeWhile { node =>
      // Left children are all the children wholly contained inside offset
      if (!measureOps.isOffsetBeforeEnd(currentIx, node.measure)) {
        currentIx = measureOps.shiftLeft(currentIx, node.measure)
        true
      } else false
    }
    val leftoverChildren = children.drop(leftChildren.length)
    leftoverChildren match {
      case Nil                       => (leftChildren, None, Nil)
      case midChild :: rightChildren =>
        // midChild is only relevant if the offset actually cuts it
        if (measureOps.isOffsetAfterBegin(currentIx, midChild.measure))
          (leftChildren, Some((currentIx, midChild)), rightChildren)
        else (leftChildren, None, leftoverChildren)
    }
  }

  /**
    * Splits the tree into 2 subtrees, semantically equivalent to
    * `(this.take(offset, measureOps), this.drop(offset, measureOps)`.
    *
    * @param offset the number of elements to take and drop.
    * @param measureOps operations to interpret this tree's measure.
    * @param measureMonoid the monoid instance for the measure.
    * @param measurable measuring operations for leaves.
    * @param treeShape the shape constraints for this tree.
    * @tparam I the type of indices used for this operation.
    * @return a pair of results of both taking and dropping `offset` elements.
    */
  def splitAt[I](
    offset: I,
    measureOps: RangeOps[I, C, M]
  )(
    implicit measureMonoid: Monoid[M],
    measurable: Measurable[C, M],
    treeShape: TreeShape
  ): (Node[C, M], Node[C, M]) = {
    value match {
      case Empty() => (this, this)
      case Leaf(elements) =>
        val left  = measureOps.take(elements, offset)
        val right = measureOps.drop(elements, offset)
        (Node(left), Node(right))
      case Internal(children) =>
        val (leftChildren, splitChild, rightChildren) =
          findIndexChild(offset, measureOps, children)
        splitChild match {
          case Some((newIx, child)) =>
            val (leftSplit, rightSplit) = child.splitAt(newIx, measureOps)

            val left  = Node.mergeTrees(leftChildren :+ leftSplit)
            val right = Node.mergeTrees(rightSplit :: rightChildren)
            (left, right)
          case None =>
            (Node.mergeTrees(leftChildren), Node.mergeTrees(rightChildren))
        }
    }
  }

  /**
    * Gets an element at a specified index.
    *
    * @param index the index to get an element at.
    * @param elemOps interpretation of indexes and elements in this tree type.
    * @tparam I the type of indices for this operation.
    * @return the element at `index`.
    */
  def get[I](
    index: I,
    elemOps: ElemOps[I, C, M]
  ): elemOps.Elem = {
    value match {
      case Empty()        => throw new ArrayIndexOutOfBoundsException
      case Leaf(elements) => elemOps.get(elements, index)
      case Internal(children) =>
        var currentIdx = index
        children
          .find { node =>
            if (elemOps.contains(currentIdx, node.measure)) true
            else {
              currentIdx = elemOps.shiftLeft(currentIdx, node.measure)
              false
            }
          }
          .getOrElse(throw new ArrayIndexOutOfBoundsException)
          .get(currentIdx, elemOps)
    }
  }
}

object Node {

  /**
    * Creates a new leaf node with provided elements.
    *
    * @param elems the elements to store.
    * @param measurable the operations for measuring the collection.
    * @tparam C the type of `elems` container.
    * @tparam M the type of measuring monoid for this tree.
    * @return a leaf node containing `elems`.
    */
  def apply[C, M](elems: C)(implicit measurable: Measurable[C, M]): Node[C, M] =
    Node(0, measurable.measure(elems), Leaf[C, M](elems))

  /**
    * An unsafe operation, creating a node with given children, without
    * checking the invariants. Should only be used if the invariants are
    * checked externally.
    *
    * @param children the children to include in the new node.
    * @return a new node containing `children`.
    */
  def unsafeFromChildren[C, M](
    children: Array[Node[C, M]]
  )(implicit measureMonoid: Monoid[M]): Node[C, M] = {
    val height = children(0).height + 1
    val size = children.foldLeft(measureMonoid.empty)((acc, n) =>
      measureMonoid.combine(acc, n.measure)
    )
    Node(height, size, Internal(children))
  }

  /**
    * An unsafe operation, creating a new node from two collections of children.
    * It does check the size invariants, but height invariants must be checked
    * externally.
    *
    * @param leftChildren the first children collection.
    * @param rightChildren the second children collection.
    * @return a tree containing both `leftChildren` and `rightChildren`
    */
  def unsafeMergeChildren[C, M](
    leftChildren: Array[Node[C, M]],
    rightChildren: Array[Node[C, M]]
  )(implicit treeShape: TreeShape, measureMonoid: Monoid[M]): Node[C, M] = {
    val allChildren = leftChildren ++ rightChildren
    val nChildren   = allChildren.length
    if (nChildren <= treeShape.maxChildren) {
      unsafeFromChildren(allChildren)
    } else {
      val splitPoint =
        math.min(treeShape.maxChildren, nChildren - treeShape.minChildren)
      val parentNodes = Array(
        Node.unsafeFromChildren(allChildren.take(splitPoint)),
        Node.unsafeFromChildren(allChildren.drop(splitPoint))
      )
      Node.unsafeFromChildren(parentNodes)
    }
  }

  /**
    * Creates an empty tree.
    *
    * @return an empty tree.
    */
  def empty[C, M](implicit measureMonoid: Monoid[M]): Node[C, M] =
    Node(0, measureMonoid.empty, Empty())

  /**
    * A safe operation for concatenating multliple valid trees into one.
    *
    * @param nodes the nodes to merge into a tree.
    * @return a new tree representing a concatenation of all the supplied
    *         trees.
    */
  def mergeTrees[C, M](
    nodes: List[Node[C, M]]
  )(implicit treeShape: TreeShape, measureMonoid: Monoid[M]): Node[C, M] =
    nodes.foldLeft(empty[C, M])((tree, node) => tree ++ node)
}
