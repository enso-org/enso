package src.main.scala.licenses.review

import java.io.FileNotFoundException
import java.nio.file.{Files, Path}

import sbt._
import src.main.scala.licenses.{
  AttachedFile,
  Attachment,
  AttachmentStatus,
  CopyrightMention,
  DependencyInformation,
  DependencySummary,
  ReviewedDependency,
  ReviewedSummary
}

import scala.util.control.NonFatal

case class Review(root: File, dependencySummary: DependencySummary) {
  def run(): ReviewedSummary = {
    val reviews = dependencySummary.dependencies.map {
      case (information, attachments) =>
        reviewDependency(information, attachments)
    }
    ReviewedSummary(reviews)
  }

  private def reviewDependency(
    info: DependencyInformation,
    attachments: Seq[Attachment]
  ): ReviewedDependency = {
    val license             = reviewLicense(info)
    val (files, copyrights) = Attachments.split(attachments)
    val copyrightsDeduplicated =
      Copyrights.removeCopyrightsIncludedInNotices(copyrights, files)
    val name = Review.normalizeName(
      info.moduleInfo.organization + "." + info.moduleInfo.name + "-" +
      info.moduleInfo.version
    )
    val packageRoot = root / name
    Files.createDirectories(packageRoot.toPath)

    val processedFiles =
      reviewFiles(packageRoot, files) ++ addFiles(packageRoot)
    val processedCopyrights =
      reviewCopyrights(packageRoot, copyrightsDeduplicated) ++
      addCopyrights(packageRoot)

    ReviewedDependency(
      information     = info,
      licenseReviewed = license,
      files           = processedFiles,
      copyrights      = processedCopyrights
    )
  }

  private def reviewFiles(
    packageRoot: File,
    files: Seq[AttachedFile]
  ): Seq[(AttachedFile, AttachmentStatus)] = {
    val ignore = readLines(packageRoot / "files-ignore")
    val keep   = readLines(packageRoot / "files-keep")
    def review(file: AttachedFile): AttachmentStatus = {
      val key = file.path.toString
      if (keep.contains(key)) AttachmentStatus.Keep
      else if (ignore.contains(key)) AttachmentStatus.Ignore
      else AttachmentStatus.NotReviewed
    }
    files.map(f => (f, review(f)))
  }

  private def addFiles(
    packageRoot: File
  ): Seq[(AttachedFile, AttachmentStatus)] = {
    listFiles(packageRoot / "files-add")
      .map(f => AttachedFile(path = f.toPath.getFileName, content = IO.read(f)))
      .map((_, AttachmentStatus.Added))
  }

  private def reviewCopyrights(
    packageRoot: File,
    copyrights: Seq[CopyrightMention]
  ): Seq[(CopyrightMention, AttachmentStatus)] = {
    val ignore      = readLines(packageRoot / "copyright-ignore")
    val keep        = readLines(packageRoot / "copyright-keep")
    val keepContext = readLines(packageRoot / "copyright-keep-context")
    def review(copyright: CopyrightMention): AttachmentStatus = {
      val key = copyright.content.strip
      if (keepContext.contains(key)) AttachmentStatus.KeepWithContext
      else if (keep.contains(key)) AttachmentStatus.Keep
      else if (ignore.contains(key)) AttachmentStatus.Ignore
      else AttachmentStatus.NotReviewed
    }
    copyrights.map(c => (c, review(c)))
  }

  private def addCopyrights(
    packageRoot: File
  ): Seq[(CopyrightMention, AttachmentStatus)] =
    readFile(packageRoot / "copyright-add")
      .map(text =>
        (
          CopyrightMention(
            content  = "<manually added mentions>",
            contexts = Seq(text),
            origins  = Seq()
          ),
          AttachmentStatus.Added
        )
      )
      .toSeq

  private def reviewLicense(
    info: DependencyInformation
  ): Option[Path] = {
    val file =
      root.getParentFile / "reviewed-licenses" /
      Review.normalizeName(info.license.name)
    readFile(file).map(Path.of(_))
  }

  private def readLines(file: File): Seq[String] =
    try { IO.readLines(file).map(_.strip) }
    catch {
      case _: FileNotFoundException =>
        IO.write(file, "")
        Seq()
    }

  private def readFile(file: File): Option[String] =
    try { Some(IO.read(file)) }
    catch { case NonFatal(_) => None }

  private def listFiles(dir: File): Seq[File] = {
    Files.createDirectories(dir.toPath)
    try { IO.listFiles(dir) }
    catch { case NonFatal(_) => Seq() }
  }
}

object Review {
  def normalizeName(string: String): String = {
    val charsToReplace = " /:;,"
    charsToReplace.foldLeft(string)((str, char) => str.replace(char, '_'))
  }
}
