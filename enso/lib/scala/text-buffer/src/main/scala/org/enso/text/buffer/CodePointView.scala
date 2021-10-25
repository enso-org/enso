package org.enso.text.buffer

/** Exposes a code points based view over rope indexing operations.
  *
  * @param rope the underlying rope.
  */
case class CodePointView(rope: Rope) {

  /** Splits the rope into two parts at a given offset.
    *
    * @param offset the number of code points in the first part.
    * @return a sub-rope containing `offset` code points and a sub-rope
    *         containing all the remaining code points.
    */
  def splitAt(offset: Int): (Rope, Rope) =
    rope.splitWith(offset, CodePointView.Ops)

  /** Takes a prefix of the rope of the given length.
    *
    * @param len the length of the prefix to take.
    * @return a prefix of `this` containing `len` code points.
    */
  def take(len: Int): Rope = rope.takeWith(len, CodePointView.Ops)

  /** Takes a suffix of the rope by removing the first `len` code points.
    *
    * @param len the number of code points to remove.
    * @return the suffix consisting of the remaining code points.
    */
  def drop(len: Int): Rope = rope.dropWith(len, CodePointView.Ops)

  /** Takes a substring of the rope, with the first code point at `startOffset`
    * and the last at `endOffset` - 1.
    *
    * @param startOffset the offset of the first code point.
    * @param endOffset the offset after the last code point.
    * @return a substring of this rope.
    */
  def substring(startOffset: Int, endOffset: Int): Rope =
    take(endOffset).codePoints.drop(startOffset)

  /** Gets the code point at a specified position.
    *
    * @param index the index of the code point to get.
    * @return the code point at `index`
    */
  def at(index: Int): Int = rope.root.get(index, CodePointView.Ops)

  /** Gets the number of code points in this rope.
    *
    * @return the length of this rope.
    */
  def length: Int = rope.measure.utf32Size
}

object CodePointView {
  object Ops
      extends RangeOps[Int, String, StringMeasure]
      with ElemOps[Int, String, StringMeasure] {
    type Elem = Int

    override def contains(
      index: Int,
      measure: StringMeasure
    ): Boolean = index < measure.utf32Size

    override def get(
      container: String,
      index: Int
    ): Int = container.codePointAt(container.offsetByCodePoints(0, index))

    override def isOffsetBeforeEnd(
      index: Int,
      measure: StringMeasure
    ): Boolean = index < measure.utf32Size

    override def isOffsetAfterBegin(
      index: Int,
      measure: StringMeasure
    ): Boolean = index > 0

    override def shiftLeft(
      index: Int,
      measure: StringMeasure
    ): Int = index - measure.utf32Size

    override def take(
      container: String,
      len: Int
    ): String = {
      val splitPoint = container.offsetByCodePoints(0, len)
      container.substring(0, splitPoint)
    }
    override def drop(
      container: String,
      len: Int
    ): String = {
      val splitPoint = container.offsetByCodePoints(0, len)
      container.substring(splitPoint, container.length)
    }
  }
}
