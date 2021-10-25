package org.enso.text.editing

import TestData._
import org.enso.text.buffer.Rope
import org.enso.text.editing.model.{Position, Range, TextEdit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RopeTextEditorSpec extends AnyFlatSpec with Matchers {

  "A rope text editor" should "insert text before beginning of a line" in {
    //given
    val beforeMain          = Range(Position(1, 0), Position(1, 0))
    val insertionBeforeMain = TextEdit(beforeMain, "ultra_")
    //when
    val result = RopeTextEditor.edit(testSnippet, insertionBeforeMain)
    //then
    result.toString mustBe """
                             |ultra_main =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    result = plusOne 10
                             |    result""".stripMargin
  }

  it should "replace a substring" in {
    //given
    val mainPosition    = Range(Position(1, 0), Position(1, 4))
    val mainReplacement = TextEdit(mainPosition, "run")
    //when
    val result = RopeTextEditor.edit(testSnippet, mainReplacement)
    //then
    result.toString mustBe """
                             |run =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    result = plusOne 10
                             |    result""".stripMargin
  }

  it should "replace a multiline substring" in {
    //given
    val resultPosition = Range(Position(5, 4), Position(6, 10))
    val change =
      """sum = plusOne 5
        |    sum""".stripMargin
    val resultReplacement = TextEdit(resultPosition, change)
    //when
    val result = RopeTextEditor.edit(testSnippet, resultReplacement)
    //then
    result.toString mustBe """
                             |main =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    sum = plusOne 5
                             |    sum""".stripMargin
  }

  it should "be able to insert change at the end of file" in {
    //given
    val eof          = Range(Position(6, 10), Position(6, 10))
    val insertedText = """
                         |    return result""".stripMargin
    val insertion    = TextEdit(eof, insertedText)
    //when
    val result = RopeTextEditor.edit(testSnippet, insertion)
    //then
    result.toString mustBe """
                             |main =
                             |    apply = v f -> f v
                             |    adder = a b -> a + b
                             |    plusOne = apply (f = adder 1)
                             |    result = plusOne 10
                             |    result
                             |    return result""".stripMargin
  }

  it should "support code points above 0xFFFF" in {
    //given
    //0x0001F4AF
    val utf32Text           = Rope("unicode: \ud83d\udcaf end")
    val positionInCodeUnits = Range(Position(0, 9), Position(0, 10))
    //0x0001F449
    val diff = TextEdit(positionInCodeUnits, "\ud83d\udc49")
    //when
    val result = RopeTextEditor.edit(utf32Text, diff)
    //then
    result.toString mustBe "unicode: \ud83d\udc49 end"
  }

}
