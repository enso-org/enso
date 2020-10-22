package org.enso.text.buffer

import cats.Monoid

/** The measure used for storing strings in the b-tree.
  *
  * @param utf16Size number of characters.
  * @param utf32Size number of code points.
  * @param fullLines number of lines terminated with a new line character.
  * @param endsInNewLine whether this range ends with a new line character.
  */
case class StringMeasure(
  utf16Size: Int,
  utf32Size: Int,
  fullLines: Int,
  endsInNewLine: Boolean
) {

  /** Number of lines measured, including the possibly non-terminated last line.
    *
    * @return the number of lines.
    */
  def linesCount: Int = fullLines + 1
}

object StringMeasure {
  implicit val monoid: Monoid[StringMeasure] = new Monoid[StringMeasure] {
    override def empty: StringMeasure = StringMeasure(0, 0, 0, false)
    override def combine(x: StringMeasure, y: StringMeasure): StringMeasure =
      StringMeasure(
        x.utf16Size + y.utf16Size,
        x.utf32Size + y.utf32Size,
        x.fullLines + y.fullLines,
        y.endsInNewLine
      )
  }
}

/** Represents a string using a tree.
  *
  * Provides fast indexing operations by lines, characters and code points,
  * as well as fast concatenation.
  *
  * The elements are stored in a [[Node]] hierarchy, with leaves storing
  * Strings, with an additional invariant that new line characters are only
  * allowed at the end of leaves.
  *
  * @param root the underlying tree's root.
  */
case class Rope(root: Node[String, StringMeasure]) {
  import Rope._

  /** Concates another rope at the end of this one.
    *
    * @param that the rope to add at the end.
    * @return the result of concatenating the two ropes.
    */
  def ++(that: Rope): Rope = Rope(this.root ++ that.root)

  /** Converts this rope to a String.
    *
    * @return a String with this rope's contents
    */
  override def toString: String = {
    val sb = new StringBuilder(root.measure.utf16Size)
    root.value.foreach { str => sb.append(str) }
    sb.toString()
  }

  private[buffer] def splitWith(
    ix: Int,
    ops: RangeOps[Int, String, StringMeasure]
  ): (Rope, Rope) = {
    val (lNode, rNode) = root.splitAt(ix, ops)
    (Rope(lNode), Rope(rNode))
  }

  private[buffer] def takeWith(
    len: Int,
    ops: RangeOps[Int, String, StringMeasure]
  ): Rope = Rope(root.take(len, ops))

  private[buffer] def dropWith(
    len: Int,
    ops: RangeOps[Int, String, StringMeasure]
  ): Rope = Rope(root.drop(len, ops))

  private[buffer] def getWith(
    index: Int,
    elemOps: ElemOps[Int, String, StringMeasure]
  ): elemOps.Elem = root.get(index, elemOps)

  /** Returns a code points based view of this rope.
    *
    * @return a code points view for this rope.
    */
  def codePoints: CodePointView = CodePointView(this)

  /**  Returns a characters view of this rope.
    *
    * @return a characters view for this rope.
    */
  def characters: CharView = CharView(this)

  /** Returns a lines view of this rope.
    *
    * @return a lines view of this rope.
    */
  def lines: LineView = LineView(this)

  private[buffer] def measure: StringMeasure = root.measure
}

object Rope {
  implicit val treeShape: TreeShape = new TreeShape {
    override val maxChildren: Int = 8
  }

  implicit val measurable: Measurable[String, StringMeasure] =
    (str: String) => {
      val endsWithNewLine = StringUtils.endsWithNewLine(str)
      StringMeasure(
        str.length,
        str.codePointCount(0, str.length),
        if (endsWithNewLine) 1 else 0,
        endsWithNewLine
      )
    }

  /** Creates a new Rope from a given string.
    *
    * @param str a string to convert into a rope.
    * @return a rope with the same contents as `str`
    */
  def apply(str: String): Rope = {
    val lines = StringUtils.getLines(str)
    val nodes = lines.map(Node(_))
    Rope(Node.mergeTrees(nodes))
  }

  /** Creates an empty rope.
    *
    * @return an empty rope.
    */
  def empty: Rope = Rope(Node.empty[String, StringMeasure])
}
