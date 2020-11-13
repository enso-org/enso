package org.enso.text.buffer

/**
  * Encodes element-level operation on a tree.
  *
  * @tparam I the type of indices.
  * @tparam C the type of leaf nodes contents.
  * @tparam M the type of measuring monoid.
  */
trait ElemOps[I, C, M] extends RangeOps[I, C, M] {

  /**
    * the type of elements associated with this interpretation of indexing.
    */
  type Elem

  /**
    * Gets an element at `index` from the `container`.
    *
    * @param container the container to look up index in.
    * @param index the index look up.
    * @return the element of `container` at `index`
    */
  def get(container: C, index: I): Elem

  /**
    * Bounds check for the index.
    *
    * @param index the index to check.
    * @param measure the size of the container according to specific measure.
    * @return whether the index is in bounds or not.
    */
  def contains(index: I, measure: M): Boolean
}

/**
  * Encodes slicing operations on a container with respect to a specific
  * measure.
  * @tparam I the type of indices.
  * @tparam C the type of leaf nodes contents.
  * @tparam M the type of measuring monoid.
  */
trait RangeOps[I, C, M] {

  /**
    * Is the offset strictly before the end of a container of a given measure?
    * e.g. if measure is the length of an array, this operation would be just
    * `offset < measure`.
    *
    * @param offset the offset to check.
    * @param measure the measure of the container.
    * @return `true` if the offset is strictly before the end of the container.
    */
  def isOffsetBeforeEnd(offset: I, measure: M): Boolean

  /**
    * Is the offset strictly after the beginning of a container of a given
    * measure?
    * e.g. if measure is the length of an array, this operation would be just
    * `offset > 0`.
    *
    * @param offset the offset to check.
    * @param measure the measure of the container.
    * @return `true` if the offset is strictly after the beginning of the
    *        container.
    */
  def isOffsetAfterBegin(offset: I, measure: M): Boolean

  /**
    * Moves the offset by a given measure. I.e. if we have a container
    * `c = c1 ++ c2`, where `c1` and `c2` have measures `m1` and `m2`
    * respectively and an offset `off` inside `c`, then `shiftLeft(off, m1)`
    * would be the offset of `off` inside `c2` alone.
    *
    * @param offset the offset to shift.
    * @param measure the measure to shift offset by.
    * @return the shifted offset.
    */
  def shiftLeft(offset: I, measure: M): I

  /**
    * Takes `len` elements from the start of `container`.
    *
    * @param container the container to take the prefix of.
    * @param len the length of the prefix.
    * @return the prefix of length `len`.
    */
  def take(container: C, len: I): C

  /**
    * Drops `len` elements from the start of `container`.
    *
    * @param container the container to take the suffix of.
    * @param len the length of the dropped prefix.
    * @return the suffix of `container` resulting from removing the first `len`
    *         elements.
    */
  def drop(container: C, len: I): C
}

/**
  * Encodes the act of measuring a container with respect to a specific
  * measure.
  *
  * @tparam C the type of measured containers.
  * @tparam M the type of measure.
  */
trait Measurable[C, M] {

  /**
    * Measures a container with respect to measure `M`.
    * @param container the container to measure.
    * @return the measure of the container.
    */
  def measure(container: C): M
}

/**
  * The tree shape constants, used to regulate the depth and width of B-Trees.
  */
trait TreeShape {

  /**
    * The maximum number of children a node can have.
    * @return the max number of children.
    */
  val maxChildren: Int

  /**
    * The minimum number of children a node can have.
    * For the tree invariants to be possible to maintain, it must be true that
    * `maxChildren + 1 >= 2 * minChildren`.
    * In practice it does not really make sense to choose a smaller value here.
    *
    *  @return the min number of children.
    */
  lazy val minChildren: Int = (maxChildren + 1) / 2
}
