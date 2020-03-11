package org.enso.languageserver.text.editing

import org.enso.languageserver.data.buffer.Rope
import org.enso.languageserver.text.editing.TextEditValidator.validate
import org.enso.languageserver.text.editing.model._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class TextEditValidatorSpec extends AnyFlatSpec with Matchers {

  "A rope text editor" should "fail when end position is before start position" in {
    val diff1 = TextEdit(Range(Position(0, 3), Position(0, 2)), "a")
    validate(buffer, diff1) mustBe Left(EndPositionBeforeStartPosition)
    val diff2 = TextEdit(Range(Position(0, 3), Position(0, 2)), "a")
    validate(buffer, diff2) mustBe Left(EndPositionBeforeStartPosition)
  }

  it should "fail when range contains negative numbers" in {
    val diff1 = TextEdit(Range(Position(-1, 3), Position(0, 2)), "a")
    validate(buffer, diff1) mustBe Left(NegativeCoordinateInPosition)
    val diff2 = TextEdit(Range(Position(0, -3), Position(0, 2)), "a")
    validate(buffer, diff2) mustBe Left(NegativeCoordinateInPosition)
  }

  it should "fail if position is outside of buffer" in {
    val diff1 = TextEdit(Range(Position(0, 3), Position(10, 2)), "a")
    validate(buffer, diff1) mustBe Left(InvalidPosition(Position(10, 2)))
    val diff2 = TextEdit(Range(Position(0, 10), Position(1, 2)), "a")
    validate(buffer, diff2) mustBe Left(InvalidPosition(Position(0, 10)))
  }

  it should "succeed when character coordinate is equal to line length" in {
    val diff1 = TextEdit(Range(Position(0, 7), Position(0, 7)), "a")
    validate(buffer, diff1) mustBe Right(())
  }

  lazy val buffer = Rope("1234567\nabcdefg")

}
