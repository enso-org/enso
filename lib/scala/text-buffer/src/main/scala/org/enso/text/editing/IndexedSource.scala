package org.enso.text.editing

import org.enso.text.buffer.Rope
import org.enso.text.editing.model.Position

/** A source which character positions can be accessed by index.
  *
  * @tparam A a source type
  */
trait IndexedSource[-A] {

  /** Converts position relative to a line to an absolute position in the
    * source.
    *
    * @param pos character position.
    * @param source the source text.
    * @return absolute position in the source.
    */
  def toIndex(pos: Position, source: A): Int

  /** Converts an absolute position to the position relative to a line.
    *
    * @param index the absolute position in the source
    * @param source the source text
    * @return the position relative to a line
    */
  def toPosition(index: Int, source: A): Position
}

object IndexedSource {

  def apply[A](implicit is: IndexedSource[A]): IndexedSource[A] = is

  implicit val CharSequenceIndexedSource: IndexedSource[CharSequence] =
    new IndexedSource[CharSequence] {

      /** @inheritdoc */
      override def toIndex(pos: Position, source: CharSequence): Int = {
        val prefix            = source.toString.linesIterator.take(pos.line)
        val lastCarrierReturn = if (pos.line > 0) 1 else 0
        prefix.mkString("\n").length + lastCarrierReturn + pos.character
      }

      /** @inheritdoc */
      override def toPosition(index: Int, source: CharSequence): Position = {
        @scala.annotation.tailrec
        def go(lx: Int, ix: Int, lines: Iterator[String]): Position = {
          if (lines.hasNext) {
            val line = lines.next()
            if (line.length < ix)
              go(lx + 1, ix - line.length - 1, lines)
            else
              Position(lx, ix)
          } else {
            Position(lx, ix)
          }
        }
        go(0, index, source.toString.linesIterator)
      }
    }

  implicit val RopeIndexedSource: IndexedSource[Rope] =
    new IndexedSource[Rope] {

      /** @inheritdoc */
      override def toIndex(pos: Position, source: Rope): Int = {
        val prefix = source.lines.take(pos.line)
        prefix.characters.length + pos.character
      }

      /** @inheritdoc */
      override def toPosition(index: Int, source: Rope): Position = {
        @scala.annotation.tailrec
        def go(lx: Int, ix: Int, source: Rope): Position = {
          val (hd, tl) = source.lines.splitAt(1)
          val length =
            if (tl.characters.length == 0) hd.characters.length
            else hd.characters.length - 1
          if (length < ix)
            go(lx + 1, ix - hd.characters.length, tl)
          else
            Position(lx, ix)
        }
        go(0, index, source)
      }
    }

}
