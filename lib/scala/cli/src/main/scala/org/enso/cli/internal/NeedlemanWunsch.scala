package org.enso.cli.internal

object NeedlemanWunsch {

  /** Computes the similarity score between two words using the Needleman-Wunsch
    * algorithm, as described at
    * https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
    *
    * Uses approximate Euclidean distance between keys on the QWERTY keyboard as
    * the mismatch score. The weights are chosen in such a way, that the smaller
    * the score, the bigger is the similarity between the two words.
    */
  def score(wordA: String, wordB: String): Double = {
    val matchScore = 0.0
    val indelScore = 2.0
    def mismatchScore(a: Char, b: Char): Double = {
      val distance = keyboardDistance(a, b)
      (Seq(2 * indelScore) ++ distance.toSeq).min
    }
    def pairingScore(a: Char, b: Char): Double =
      if (a == b) matchScore else mismatchScore(a, b)

    val table = Array.ofDim[Double](wordA.length + 1, wordB.length + 1)

    table(0)(0) = 0.0
    for (i <- 1 to wordA.length) {
      table(i)(0) = table(i - 1)(0) + indelScore
    }
    for (j <- 1 to wordB.length) {
      table(0)(j) = table(0)(j - 1) + indelScore
    }

    for (i <- 1 to wordA.length) {
      for (j <- 1 to wordB.length) {
        val indelI = table(i - 1)(j) + indelScore
        val indelJ = table(i)(j - 1) + indelScore
        val pairing =
          table(i - 1)(j - 1) + pairingScore(wordA(i - 1), wordB(j - 1))
        val best = Seq(indelI, indelJ, pairing).min
        table(i)(j) = best
      }
    }

    table(wordA.length)(wordB.length)
  }

  private val keyboardLower: Seq[(Double, String)] = Seq(
    (0, "`1234567890-="),
    (1.5, "qwertyuiop[]\\"),
    (2, "asdfghjkl;'"),
    (2.5, "zxcvbnm,./")
  )
  private val keyboardUpper: Seq[(Double, String)] = Seq(
    (0, "~!@#$%^&*()_+"),
    (1.5, "QWERTYUIOP{}|"),
    (2, "ASDFGHJKL:"),
    (2.5, "ZXCVBNM<>?")
  )

  private val keyboardMap: Map[Char, (Double, Double)] = {
    def computePositions(
      rows: Seq[(Double, String)]
    ): Seq[(Char, (Double, Double))] =
      rows.zipWithIndex flatMap { case ((paddingLeft, chars), row) =>
        chars.zipWithIndex map { case (char, col) =>
          char -> ((paddingLeft + col, 0.0 + row))
        }
      }

    val tuples: Seq[(Char, (Double, Double))] =
      computePositions(keyboardLower) ++ computePositions(keyboardUpper)

    Map.from(tuples)
  }

  /** Approximates distance between two keys on a QWERTY keyboard. If a provided
    * character is not contained in the set of basic keys, None is returned.
    */
  def keyboardDistance(from: Char, to: Char): Option[Double] =
    for {
      (x1, y1) <- keyboardMap.get(from)
      (x2, y2) <- keyboardMap.get(to)
      dx = x2 - x1
      dy = y2 - y1
    } yield Math.sqrt(dx * dx + dy * dy)
}
