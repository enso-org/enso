package src.main.scala.licenses.backend

import java.nio.file.{Files, Path}

import src.main.scala.licenses.{Attachment, Notice}

object GatherNotices extends AttachmentGatherer {
  def run(root: Path): Seq[Attachment] = {
    AttachmentGatherer.walk(root) { path =>
      if (Files.isRegularFile(path) && mayBeNotice(path)) {
        Seq(Notice.read(path, Some(root)))
      } else Seq()
    }
  }

  private def mayBeNotice(path: Path): Boolean = {
    val fileName = path.getFileName.toString.toLowerCase
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

  private val ignoredExtensions = Seq("scala", "java")
  private val possibleNames =
    Seq("notice", "copyright", "thirdparty", "license", "credit")
}
