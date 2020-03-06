package org.enso.languageserver.text
import org.enso.languageserver.data.buffer.Rope
import org.scalacheck.Gen.Parameters
import org.scalameter.{Bench, Gen}

object RopeBench extends Bench.LocalTime {
  val numLines: Gen[Int] = Gen.range("lines")(10000, 100000, 30000)
  val baseLine =
    "abcdeftnrbgtnrbgerfsv  bnoebnoenbodvnogjeiojhnbgrwfegernboinb onvioernviorngbofds. + 1 envioergiogjerogie  rjg7ioerjoirvocaqwqrwettrwre"

  def genLines(num: Int): String = {
    val maxLen = 127
    var len    = 13
    val sb     = new StringBuilder
    0.until(num).foreach { _ =>
      len = (len * 17 + 37) % maxLen
      sb.addAll(baseLine.substring(0, len) + "\n")
    }
    sb.toString()
  }

  val text: Gen[String] = numLines.map(genLines)
  val rope: Gen[Rope]   = text.map(Rope.apply)

  performance of "Rope" in {
    measure method "constructor" in {
      using(text) in { text =>
        Rope(text)
      }
    }

    measure method "++" in {
      using(rope) in { rope =>
        rope ++ rope
      }
    }

    measure method "characters.split" in {
      using(rope) in { rope =>
        rope.characters.splitAt(rope.characters.length / 2)
      }
    }

    measure method "codePoints.split" in {
      using(rope) in { rope =>
        rope.codePoints.splitAt(rope.codePoints.length / 2)
      }
    }

    measure method "lines.split" in {
      using(rope) in { rope =>
        rope.lines.splitAt(rope.lines.length / 2)
      }
    }
  }
}
