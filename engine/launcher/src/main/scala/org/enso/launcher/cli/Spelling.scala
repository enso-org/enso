package org.enso.launcher.cli

object Spelling {
  def selectClosestMatches(
    typo: String,
    possibleWords: Seq[String]
  ): Seq[String] =
    selectClosestMatchesWithMetadata(typo, possibleWords.map((_, ()))).map(_._1)

  private val maxScoreThreshold: Double = 10.0
  private val topSelectionsLimit        = 3
  def selectClosestMatchesWithMetadata[A](
    typo: String,
    possibleWords: Seq[(String, A)]
  ): Seq[(String, A)] =
    if (
      possibleWords.isEmpty ||
      typo.length > 2 * possibleWords.map(_._1.length).max
    ) Seq()
    else {
      val withScores =
        possibleWords.map(word => (scoreNeedlemanWunsch(typo, word._1), word))
      val top = withScores
        .filter(_._1 < maxScoreThreshold)
        .sortBy(_._1)
        .take(topSelectionsLimit)
      top.map(_._2)
    }

  /**
    * Compute the similarity score between two words using the Needleman-Wunsch
    * algorithm, as described at
    * https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
    *
    * Uses approximate Euclidean distance between keys on the QWERTY keyboard as
    * the mismatch score. The weights are chosen in such a way, that the smaller
    * the score, the bigger is the similarity between the two words.
    */
  def scoreNeedlemanWunsch(wordA: String, wordB: String): Double = {
    val matchScore = 0.0
    val indelScore = 2.0
    def mismatchScore(a: Char, b: Char): Double = {
      val distance = keyboardDistance(a, b)
      (Seq(2 * indelScore) ++ distance.toSeq).min
    }
    def pairingScore(a: Char, b: Char): Double =
      if (a == b) matchScore else mismatchScore(a, b)

    def make2dArray(width: Int, height: Int): Array[Array[Double]] = {
      val result = new Array[Array[Double]](width)
      for (i <- 0 until width) {
        result(i) = new Array[Double](height)
      }
      result
    }

    val table = make2dArray(wordA.length + 1, wordB.length + 1)

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
      rows.zipWithIndex flatMap {
        case ((paddingLeft, chars), row) =>
          chars.zipWithIndex map {
            case (char, col) =>
              char -> ((paddingLeft + col, 0.0 + row))
          }
      }

    val tuples: Seq[(Char, (Double, Double))] =
      computePositions(keyboardLower) ++ computePositions(keyboardUpper)

    Map.from(tuples)
  }

  def keyboardDistance(from: Char, to: Char): Option[Double] =
    for {
      (x1, y1) <- keyboardMap.get(from)
      (x2, y2) <- keyboardMap.get(to)
      dx = x2 - x1
      dy = y2 - y1
    } yield Math.sqrt(dx * dx + dy * dy)
}
