package org.enso.text

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object Generators {
  val newLinedStrings: Gen[List[String]] = {
    val stringWithNewline: Gen[String] = for {
      str         <- arbitrary[String]
      inclNewLine <- arbitrary[Boolean]
      codePointsLen = str.codePointCount(0, str.length)
      newLinePos <- Gen.choose(0, codePointsLen)
      newLine    <- Gen.oneOf("\n", "\r\n")
    } yield
      if (inclNewLine) {
        val newLinePosUtf = str.offsetByCodePoints(0, newLinePos)
        str.substring(0, newLinePosUtf) + newLine + str.substring(
          newLinePosUtf,
          str.length
        )
      } else {
        str
      }
    Gen.listOf(stringWithNewline)
  }

  val newLinedString: Gen[String] = newLinedStrings.map(_.mkString(""))
}
