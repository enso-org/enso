package src.main.scala.licenses.backend
import java.nio.file.{Files, Path}

import sbt.IO
import src.main.scala.licenses.{Attachment, CopyrightMention, FilesHelper}

import scala.util.control.NonFatal

/**
  * The algorithm for gathering any copyright notices inside of source files.
  *
  * It reads all text files and gathers any lines that contain the word
  * copyright - it may introduce some false positives but these can be manually
  * reviewed and ignored.
  *
  * It tries to include context in which the line has appeared, first by trying
  * to detect if the line is a part of a longer comment; if the first method
  * fails, it just shows 2 lines before and after the selected one.
  */
object GatherCopyrights extends AttachmentGatherer {

  /**
    * @inheritdoc
    */
  override def run(root: Path): Seq[Attachment] = {
    val allCopyrights = FilesHelper.walk(root) { path =>
      if (Files.isRegularFile(path)) {
        val relativePath = root.relativize(path)
        try {
          val lines = IO.readLines(path.toFile).zipWithIndex.toIndexedSeq
          lines
            .filter(l => mayBeCopyright(l._1))
            .map { case (str, idx) => (str, findContext(lines)(idx)) }
            .map {
              case (line, context) =>
                CopyrightMention.from(
                  CopyrightMention.cleanup(line),
                  Seq(context),
                  Seq(relativePath)
                )
            }
        } catch {
          case NonFatal(e) =>
            Seq(
              CopyrightMention.from(
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

  /**
    * Decides if the line may contain a copyright.
    */
  private def mayBeCopyright(line: String): Boolean =
    line.toLowerCase.contains("copyright")

  /**
    * Finds context of the given line.
    *
    * If the selected line seems to be a part of a block comment, the context
    * becomes the whole comment, otherwise it just includes 2 lines before and
    * after the selected line.
    *
    * @param allLines list of all lines available (that will be searched for
    *                 context)
    * @param idx index of the selected line in `allLines`
    * @return a string representing the found context
    */
  private def findContext(
    allLines: IndexedSeq[(String, Int)]
  )(idx: Int): String = {
    val (line, _) = allLines(idx)
    val nearbyLines: Seq[String] = if (line.stripLeading().startsWith("*")) {
      val start = allLines
        .take(idx + 1)
        .filter(_._1.contains("/*"))
        .map(_._2)
        .lastOption
        .getOrElse(idx - 2)
      val end = allLines
        .drop(idx)
        .filter(_._1.contains("*/"))
        .map(_._2)
        .headOption
        .getOrElse(idx + 2)
      allLines.slice(start, end + 1).map(_._1)
    } else if (
      possiblePrefixes.exists(s => line.stripLeading().headOption.contains(s))
    ) {
      val maxLinesOnSide = 20
      val prefix =
        line.takeWhile(c => c.isWhitespace || possiblePrefixes.contains(c))
      val before = allLines
        .take(idx)
        .reverse
        .takeWhile(_._1.startsWith(prefix))
        .take(maxLinesOnSide)
        .reverse
      val after = allLines
        .drop(idx)
        .takeWhile(_._1.startsWith(prefix))
        .take(maxLinesOnSide)
      (before ++ after).map(_._1)
    } else {
      allLines.slice(idx - 2, idx + 3).map(_._1)
    }
    nearbyLines.mkString("\n")
  }

  private val possiblePrefixes = Seq('-', '#', ';', '/')
}
