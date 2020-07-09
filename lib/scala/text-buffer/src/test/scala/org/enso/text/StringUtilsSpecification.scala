package org.enso.text

import org.enso.text.buffer.StringUtils
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class StringUtilsSpecification extends Properties("StringUtils") {
  property("getLines preserves string contents") =
    forAll(Generators.newLinedString) { str =>
      val lines = StringUtils.getLines(str)
      lines.mkString("") == str
    }

  property(
    "getLines result has newlines at the end of each line except the last"
  ) = forAll(Generators.newLinedString) { str =>
    val allLines = StringUtils.getLines(str)
    val lines    = allLines.take(allLines.length - 1)
    lines.forall(StringUtils.endsWithNewLine)
  }

  property("getLines result has no newlines except for line ends") =
    forAll(Generators.newLinedString) { str =>
      val lines                = StringUtils.getLines(str)
      val linesWithoutNewlines = lines.map(StringUtils.stripNewline)
      linesWithoutNewlines.forall(_.indexOf('\n') == -1)
    }
}
