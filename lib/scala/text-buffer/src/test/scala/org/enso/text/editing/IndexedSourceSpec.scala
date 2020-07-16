package org.enso.text.editing

import org.enso.text.buffer.Rope
import org.enso.text.editing.model.Position
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

sealed abstract class IndexedSourceSpec[A: IndexedSource]
    extends AnyFlatSpec
    with Matchers {

  val text: String =
    """main =
      |    x = 7
      |    y = 42
      |    IO.println(x + y)""".stripMargin.linesIterator.mkString("\n")

  implicit def toIndexedSource(text: String): A

  it should "convert position of the beginning of text" in {
    IndexedSource[A].toIndex(Position(0, 0), text) shouldEqual 0
    IndexedSource[A].toPosition(0, text) shouldEqual Position(0, 0)
  }

  it should "convert a position on a first line" in {
    IndexedSource[A].toIndex(Position(0, 5), text) shouldEqual 5
    IndexedSource[A].toPosition(5, text) shouldEqual Position(0, 5)
  }

  it should "convert last position on a first line" in {
    IndexedSource[A].toIndex(Position(0, 6), text) shouldEqual 6
    IndexedSource[A].toPosition(6, text) shouldEqual Position(0, 6)
  }

  it should "convert first position on a line" in {
    IndexedSource[A].toIndex(Position(2, 0), text) shouldEqual 17
    IndexedSource[A].toPosition(17, text) shouldEqual Position(2, 0)
  }

  it should "convert a position on a line" in {
    IndexedSource[A].toIndex(Position(2, 5), text) shouldEqual 22
    IndexedSource[A].toPosition(22, text) shouldEqual Position(2, 5)
  }

  it should "convert last position on a line" in {
    IndexedSource[A].toIndex(Position(2, 10), text) shouldEqual 27
    IndexedSource[A].toPosition(27, text) shouldEqual Position(2, 10)
  }

  it should "convert to index last position" in {
    IndexedSource[A].toIndex(Position(3, 21), text) shouldEqual 49
    IndexedSource[A].toPosition(49, text) shouldEqual Position(3, 21)
  }

}

class CharSequenceIndexedSourceSpec extends IndexedSourceSpec[CharSequence] {
  implicit override def toIndexedSource(text: String): CharSequence = text
}

class RopeIndexedSourceSpec extends IndexedSourceSpec[Rope] {
  implicit override def toIndexedSource(text: String): Rope = Rope(text)
}
