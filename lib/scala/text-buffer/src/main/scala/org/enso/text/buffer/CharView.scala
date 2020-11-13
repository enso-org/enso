package org.enso.text.buffer

/**
  * Exposes a character-based API for rope operations.
  * Also exposes a [[CharSequence]] interface, for no-copy use with Java APIs.
  *
  * @param rope the underlying rope.
  */
case class CharView(rope: Rope) extends CharSequence {

  /**
    * Splits the rope into two parts at a given offset.
    *
    * @param offset the number of characters in the first part.
    * @return a sub-rope containing `offset` characters and a sub-rope
    *         containing all the remaining characters.
    */
  def splitAt(offset: Int): (Rope, Rope) =
    rope.splitWith(offset, CharView.Ops)

  /**
    * Takes a prefix of the rope of the given length.
    *
    * @param len the length of the prefix to take.
    * @return a prefix of `this` containing `len` characters.
    */
  def take(len: Int): Rope = rope.takeWith(len, CharView.Ops)

  /**
    * Takes a suffix of the rope by removing the first `len` characters.
    *
    * @param len the number of characters to remove.
    * @return the suffix consisting of the remaining characters.
    */
  def drop(len: Int): Rope = rope.dropWith(len, CharView.Ops)

  /**
    * Takes a substring of the rope, with the first character at `startOffset`
    * and the last at `endOffset` - 1.
    *
    * @param startOffset the offset of the first character.
    * @param endOffset the offset after the last character.
    * @return a substring of this rope.
    */
  def substring(startOffset: Int, endOffset: Int): Rope =
    drop(startOffset).characters.take(endOffset - startOffset)

  /**
    * Gets the number of characters in this rope.
    *
    * @return the length of this rope.
    */
  override def length: Int = rope.root.measure.utf16Size

  /**
    * Gets the character at a specified position.
    *
    * @param index the index of the character to get.
    * @return the character at `index`
    */
  override def charAt(index: Int): Char = rope.getWith(index, CharView.Ops)

  /**
    * Takes a substring of the rope, with the first character at `start`
    * and the last at `end` - 1.
    *
    * @param start the offset of the first character.
    * @param end the offset after the last character.
    * @return a substring of this rope.
    */
  override def subSequence(
    start: Int,
    end: Int
  ): CharSequence = CharView(substring(start, end))

  override def toString: String = rope.toString
}

object CharView {
  object Ops
      extends RangeOps[Int, String, StringMeasure]
      with ElemOps[Int, String, StringMeasure] {
    type Elem = Char

    override def get(
      container: String,
      index: Int
    ): Char = container(index)

    override def contains(
      index: Int,
      measure: StringMeasure
    ): Boolean = index < measure.utf16Size

    override def isOffsetBeforeEnd(
      index: Int,
      measure: StringMeasure
    ): Boolean =
      index < measure.utf16Size

    override def isOffsetAfterBegin(
      index: Int,
      measure: StringMeasure
    ): Boolean = index > 0

    override def shiftLeft(index: Int, measure: StringMeasure): Int =
      index - measure.utf16Size

    override def take(
      container: String,
      len: Int
    ): String = container.substring(0, len)

    override def drop(
      container: String,
      len: Int
    ): String = container.substring(len, container.length)
  }
}
