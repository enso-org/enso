package org.enso.launcher.cli.internal

import cats.data.NonEmptyList

private[cli] object CLIOutputInternal {
  sealed trait TextEntity
  case class TextLine(line: String)                 extends TextEntity
  case class TextTable(rows: Seq[(String, String)]) extends TextEntity
  def groupTables(lines: Iterable[String]): Seq[TextEntity] = {
    var result: List[TextEntity]                 = Nil
    var tableAccumulator: List[(String, String)] = Nil
    def flushAccumulator(): Unit = {
      if (tableAccumulator.nonEmpty) {
        result ::= TextTable(tableAccumulator.reverse)
        tableAccumulator = Nil
      }
    }
    object tableRow {
      def unapply(line: String): Option[(String, String)] = {
        val index = line.indexOf("\t")
        if (index < 0) {
          None
        } else {
          Some((line.take(index), line.drop(index + 1)))
        }
      }
    }

    for (line <- lines) {
      line match {
        case tableRow(prefix, suffix) =>
          tableAccumulator = (prefix, suffix) :: tableAccumulator
        case normalLine =>
          flushAccumulator()
          result ::= TextLine(normalLine)
      }
    }

    flushAccumulator()
    result.reverse
  }

  def findWrapPoint(line: String, wrapLength: Int): Int = {
    if (line.length <= wrapLength) line.length
    else {
      val potentialWrapPoints =
        line.take(wrapLength + 1).zipWithIndex.filter(_._1.isWhitespace)
      potentialWrapPoints.lastOption.map(_._2).getOrElse(line.length)
    }
  }

  def wrapLine(line: String, wrapLength: Int): NonEmptyList[String] =
    if (line.length <= wrapLength) NonEmptyList.one(line)
    else {
      val wrapPoint = findWrapPoint(line, wrapLength)
      val prefix    = line.take(wrapPoint)
      val suffix    = line.drop(wrapPoint + 1)
      if (suffix.isBlank) NonEmptyList.one(prefix)
      else prefix :: wrapLine(suffix, wrapLength)
    }

  def alignAndWrapTable(
    rows: Seq[(String, String)],
    wrapLength: Int,
    minimumWrapWidth: Int,
    minimumTableWidth: Int
  ): Seq[String] = {
    val prefixLengths = rows.map(_._1.length)
    val commmonPrefixLength =
      Seq(prefixLengths.max + 1, minimumTableWidth).max
    val commonSuffixLength =
      Seq(wrapLength - commmonPrefixLength, minimumWrapWidth).max
    val additionalLinesPadding = " " * commmonPrefixLength

    rows.flatMap {
      case (prefix, suffix) =>
        val prefixPadded  = rightPad(prefix, commmonPrefixLength)
        val wrappedSuffix = wrapLine(suffix, commonSuffixLength)
        val firstLine     = prefixPadded + wrappedSuffix.head
        val restLines     = wrappedSuffix.tail.map(additionalLinesPadding + _)
        Seq(firstLine) ++ restLines
    }
  }

  def rightPad(str: String, desiredLength: Int): String =
    if (str.length > desiredLength) str
    else {
      val padding = " " * (desiredLength - str.length)
      str + padding
    }
}
