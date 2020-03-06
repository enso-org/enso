package org.enso.languageserver.text
import org.enso.languageserver.data.buffer.Rope
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary._
import org.scalacheck.Properties

object RopeSpecification extends Properties("Rope") {

  private def ropeFromStrings(strings: List[String]): Rope =
    strings.foldLeft(Rope.empty)((rope, string) => rope ++ Rope(string))

  private def normalizeOffset(offset: Int, length: Int): Int =
    (offset % (length + 1)).abs

  private def normalizeIndex(index: Int, length: Int): Int =
    (index % length).abs

  // === Rope ================================================================

  property("constructor does not modify the string") =
    forAll(Generators.newLinedString) { string =>
      Rope(string).toString == string
    }

  property("++ is consistent with strings") = forAll { strings: List[String] =>
    val fromRope = ropeFromStrings(strings).toString
    val naive    = strings.mkString("")
    fromRope == naive
  }

  // === Characters ==========================================================

  property("characters.take is consistent with strings") = forAll {
    (strings: List[String], count: Int) =>
      val naiveStr     = strings.mkString("")
      val alignedCount = normalizeOffset(count, naiveStr.length)
      val fromRope =
        ropeFromStrings(strings).characters.take(alignedCount).toString
      fromRope == naiveStr.substring(0, alignedCount)
  }

  property("characters.drop is consistent with strings") = forAll {
    (strings: List[String], count: Int) =>
      val naiveStr     = strings.mkString("")
      val alignedCount = normalizeOffset(count, naiveStr.length)
      val fromRope =
        ropeFromStrings(strings).characters.drop(alignedCount).toString
      fromRope == naiveStr.substring(alignedCount, naiveStr.length)
  }

  property("characters.split is consistent with strings") = forAll {
    (strings: List[String], count: Int) =>
      val naiveStr = strings.mkString("")
      val offset   = normalizeOffset(count, naiveStr.length)
      val (fromRopeL, fromRopeR) =
        ropeFromStrings(strings).characters.splitAt(offset)
      val fromRope = (fromRopeL.toString, fromRopeR.toString)
      val fromString = (
        naiveStr.substring(0, offset),
        naiveStr.substring(offset, naiveStr.length)
      )
      fromRope == fromString
  }

  property("characters.charAt is consistent with strings") = forAll {
    (strings: List[String], ix: Int) =>
      val naiveStr = strings.mkString("")
      val rope     = ropeFromStrings(strings)
      if (naiveStr.length > 0) {
        val index = normalizeIndex(ix, naiveStr.length)
        rope.characters.charAt(index) == naiveStr.charAt(index)
      } else true
  }

  // === CodePoints ==========================================================

  property("codePoints.take is consistent with strings") = forAll {
    (strings: List[String], count: Int) =>
      val naiveStr = strings.mkString("")
      val offset =
        normalizeOffset(count, naiveStr.codePointCount(0, naiveStr.length))
      val fromRope = ropeFromStrings(strings).codePoints.take(offset).toString
      val fromString =
        naiveStr.substring(0, naiveStr.offsetByCodePoints(0, offset))
      fromRope == fromString
  }

  property("codePoints.drop is consistent with strings") = forAll {
    (strings: List[String], count: Int) =>
      val naiveStr = strings.mkString("")
      val offset =
        normalizeOffset(count, naiveStr.codePointCount(0, naiveStr.length))
      val fromRope = ropeFromStrings(strings).codePoints.drop(offset).toString
      val fromString = naiveStr.substring(
        naiveStr.offsetByCodePoints(0, offset),
        naiveStr.length
      )
      fromRope == fromString
  }

  property("codePoints.split is consistent with strings") = forAll {
    (strings: List[String], count: Int) =>
      val naiveStr = strings.mkString("")
      val offset =
        normalizeOffset(count, naiveStr.codePointCount(0, naiveStr.length))
      val (fromRopeL, fromRopeR) =
        ropeFromStrings(strings).codePoints.splitAt(offset)
      val fromRope   = (fromRopeL.toString, fromRopeR.toString)
      val splitPoint = naiveStr.offsetByCodePoints(0, offset)
      val fromString = (
        naiveStr.substring(0, splitPoint),
        naiveStr.substring(splitPoint, naiveStr.length)
      )
      fromRope == fromString
  }

  property("codePoints.at is consistent with strings") = forAll {
    (strings: List[String], ix: Int) =>
      val naiveStr = strings.mkString("")
      val rope     = ropeFromStrings(strings)
      if (naiveStr.length > 0) {
        val index =
          normalizeIndex(ix, naiveStr.codePointCount(0, naiveStr.length))
        val codePoint =
          naiveStr.codePointAt(naiveStr.offsetByCodePoints(0, index))
        rope.codePoints.at(index) == codePoint
      } else true
  }

  // === Lines ===============================================================

  property("lines.take is consistent with list-based operations") =
    forAll(Generators.newLinedStrings, arbitrary[Int]) { (str, len) =>
      val rope       = ropeFromStrings(str)
      val mockBuffer = MockBuffer(str.mkString(""))
      val offset     = normalizeOffset(len, str.length)

      rope.lines.take(offset).toString == mockBuffer.take(offset).toString
    }

  property("lines.drop is consistent with list-based operations") =
    forAll(Generators.newLinedStrings, arbitrary[Int]) { (str, len) =>
      val rope       = ropeFromStrings(str)
      val mockBuffer = MockBuffer(str.mkString(""))
      val offset     = normalizeOffset(len, str.length)

      rope.lines.drop(offset).toString == mockBuffer.drop(offset).toString
    }

  property("lines.split is consistent with list-based operations") =
    forAll(Generators.newLinedStrings, arbitrary[Int]) { (str, len) =>
      val offset     = normalizeOffset(len, str.length)
      val rope       = ropeFromStrings(str)
      val mockBuffer = MockBuffer(str.mkString(""))

      val (ropeL, ropeR) = rope.lines.splitAt(offset)
      val leftCorrect    = ropeL.toString == mockBuffer.take(offset).toString
      val rightCorrect   = ropeR.toString == mockBuffer.drop(offset).toString

      leftCorrect && rightCorrect
    }

}
