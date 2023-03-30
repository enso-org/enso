package org.enso.text.editing

import org.enso.text.buffer.Rope
import org.enso.text.editing.model.{Position, Range, TextEdit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import TextEditValidator.validate

class TextEditValidatorSpec extends AnyFlatSpec with Matchers {

  "A rope text editor" should "fail when end position is before start position" in {
    val diff1 = TextEdit(Range(Position(0, 3), Position(0, 2)), "a")
    validate(buffer, diff1) mustBe Left(
      EndPositionBeforeStartPosition(Position(0, 3), Position(0, 3))
    )
    val diff2 = TextEdit(Range(Position(0, 3), Position(0, 2)), "a")
    validate(buffer, diff2) mustBe Left(
      EndPositionBeforeStartPosition(Position(0, 3), Position(0, 3))
    )
  }

  it should "fail when range contains negative numbers" in {
    val diff1 = TextEdit(Range(Position(-1, 3), Position(0, 2)), "a")
    validate(buffer, diff1) mustBe Left(
      NegativeCoordinateInPosition(-1, "text edit start line")
    )
    val diff2 = TextEdit(Range(Position(0, -3), Position(0, 2)), "a")
    validate(buffer, diff2) mustBe Left(
      NegativeCoordinateInPosition(-3, "text edit start character")
    )
  }

  it should "fail if position is outside of buffer" in {
    val diff1 = TextEdit(Range(Position(0, 3), Position(10, 2)), "a")
    validate(buffer, diff1) mustBe Left(
      InvalidPosition(
        Position(10, 2),
        "text edit end line (10) outside of buffer's line count (2)"
      )
    )
    val diff2 = TextEdit(Range(Position(0, 10), Position(1, 2)), "a")
    validate(buffer, diff2) mustBe Left(
      InvalidPosition(
        Position(0, 10),
        " character (10) is outside of line's length (8)"
      )
    )
  }

  it should "succeed when character coordinate is equal to line length" in {
    val diff1 = TextEdit(Range(Position(0, 7), Position(0, 7)), "a")
    validate(buffer, diff1) mustBe Right(())
  }

  lazy val buffer = Rope("1234567\nabcdefg")

}
