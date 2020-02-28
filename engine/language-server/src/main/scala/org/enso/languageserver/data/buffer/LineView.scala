package org.enso.languageserver.data.buffer

/**
  * Exposes a line-based API for the rope.
  * @param rope the underlying rope.
  */
case class LineView(rope: Rope) {

  /**
    * Takes a prefix of the rope of the given length.
    *
    * @param len the length of the prefix to take.
    * @return a prefix of `this` containing `len` lines.
    */
  def take(len: Int): Rope = rope.takeWith(len, LineView.Ops)

  /**
    * Takes a suffix of the rope by removing the first `len` lines.
    *
    * @param len the number of lines to remove.
    * @return the suffix consisting of the remaining lines.
    */
  def drop(len: Int): Rope = rope.dropWith(len, LineView.Ops)

  /**
    * Splits the rope into two parts at a given offset.
    *
    * @param offset the number of lines in the first part.
    * @return a sub-rope containing `offset` lines and a sub-rope
    *         containing all the remaining lines.
    */
  def splitAt(offset: Int): (Rope, Rope) = rope.splitWith(offset, LineView.Ops)

  /**
    * Gets the number of lines in this rope.
    *
    * @return the length of this rope.
    */
  def length: Int = rope.measure.linesCount
}

object LineView {
  object Ops extends RangeOps[Int, String, StringMeasure] {
    override def isOffsetBeforeEnd(
      index: Int,
      measure: StringMeasure
    ): Boolean =
      (index < measure.fullLines || (index == measure.fullLines && !measure.endsInNewLine))

    override def isOffsetAfterBegin(
      index: Int,
      measure: StringMeasure
    ): Boolean = index > 0

    override def shiftLeft(
      index: Int,
      measure: StringMeasure
    ): Int = index - measure.fullLines

    override def take(
      container: String,
      len: Int
    ): String = if (len == 0) "" else container

    override def drop(
      container: String,
      len: Int
    ): String = if (len == 0) container else ""
  }
}
