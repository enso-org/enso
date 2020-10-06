package src.main.scala.licenses.backend
import java.nio.file.{Files, Path}

import sbt.IO
import src.main.scala.licenses.{Attachment, CopyrightMention}

import scala.util.control.NonFatal

object GatherCopyrights extends AttachmentGatherer {
  override def run(root: Path): Seq[Attachment] = {
    val allCopyrights = AttachmentGatherer.walk(root) { path =>
      if (Files.isRegularFile(path)) {
        val relativePath = root.relativize(path)
        try {
          val lines = IO.readLines(path.toFile).zipWithIndex.toIndexedSeq
          // TODO [RW] it would be good to add the contexts as they help judge
          //  if the copyright notice is complete
          lines
            .filter(l => mayBeCopyright(l._1))
            .map { case (str, idx) => (str, findContext(lines)(idx)) }
            .map {
              case (line, context) =>
                CopyrightMention(line, Seq(context), Seq(relativePath))
            }
            .map(mention => mention.copy(content = cleanup(mention.content)))
        } catch {
          case NonFatal(e) =>
            Seq(
              CopyrightMention(
                "<some files could not be read>",
                Seq(e.toString),
                Seq(relativePath)
              )
            )
        }
      } else {
        Seq()
      }
    }
    CopyrightMention.mergeByContext(
      CopyrightMention.mergeByContent(allCopyrights)
    )
  }

  private def mayBeCopyright(line: String): Boolean =
    line.toLowerCase.contains("copyright")

  private def findContext(
    allLines: IndexedSeq[(String, Int)]
  )(pos: Int): String = {
    val (line, _) = allLines(pos)
    val nearbyLines: Seq[String] = if (line.stripLeading().startsWith("*")) {
      val start = allLines
        .take(pos + 1)
        .filter(_._1.contains("/*"))
        .map(_._2)
        .lastOption
        .getOrElse(pos - 2)
      val end = allLines
        .drop(pos)
        .filter(_._1.contains("*/"))
        .map(_._2)
        .headOption
        .getOrElse(pos + 2)
      allLines.slice(start, end + 1).map(_._1)
    } else if (
      possiblePrefixes.exists(s => line.stripLeading().headOption.contains(s))
    ) {
      val maxLinesOnSide = 20
      val prefix =
        line.takeWhile(c => c.isWhitespace || possiblePrefixes.contains(c))
      val before = allLines
        .take(pos)
        .reverse
        .takeWhile(_._1.startsWith(prefix))
        .take(maxLinesOnSide)
        .reverse
      val after = allLines
        .drop(pos)
        .takeWhile(_._1.startsWith(prefix))
        .take(maxLinesOnSide)
      (before ++ after).map(_._1)
    } else {
      allLines.slice(pos - 2, pos + 3).map(_._1)
    }
    nearbyLines.mkString("\n")
  }

  private val possiblePrefixes = Seq('-', '#', ';', '/')

  private def cleanup(string: String): String = {
    val charsToIgnore = Seq('*', '-', '#', '/')
    string.dropWhile(char => char.isWhitespace || charsToIgnore.contains(char))
  }
}
