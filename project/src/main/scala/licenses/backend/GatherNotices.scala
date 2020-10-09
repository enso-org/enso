package src.main.scala.licenses.backend

import java.nio.file.{Files, Path}

import src.main.scala.licenses.{AttachedFile, Attachment}

/**
  * The algorithm for gathering any copyright-related files found in the
  * sources.
  *
  * It gathers any files whose name contains one of the keywords from
  * `possibleNames`, but filters out ordinary source files.
  */
object GatherNotices extends AttachmentGatherer {

  /**
    * @inheritdoc
    */
  override def run(root: Path): Seq[Attachment] = {
    AttachmentGatherer.walk(root) { path =>
      if (Files.isRegularFile(path) && mayBeRelevant(path)) {
        Seq(AttachedFile.read(path, Some(root)))
      } else Seq()
    }
  }

  /**
    * Decides if the path may be relevant and should be included in the result.
    */
  def mayBeRelevant(fileName: String): Boolean = {
    val extension = {
      val lastDot = fileName.lastIndexOf(".")
      if (lastDot < 0) None
      else Some(fileName.substring(lastDot + 1))
    }
    if (extension.exists(ignoredExtensions.contains)) {
      false
    } else {
      val simplifiedName = fileName.filter(_.isLetterOrDigit)
      possibleNames.exists(simplifiedName.contains)
    }
  }

  /**
    * Decides if the filename is relevant and should be included in the result.
    */
  def mayBeRelevant(path: Path): Boolean =
    mayBeRelevant(path.getFileName.toString.toLowerCase)

  /**
    * File extensions that are ignored.
    *
    * Source files are ignored because they are scanned for copyright notices by
    * [[GatherCopyrights]], they are not likely to constitute separate copyright
    * files.
    */
  private val ignoredExtensions = Seq("scala", "java")

  /**
    * File name keywords that indicate that a file should be included.
    */
  val possibleNames =
    Seq(
      "notice",
      "copyright",
      "thirdparty",
      "license",
      "licence",
      "credit",
      "copying",
      "authors"
    )
}
