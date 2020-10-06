package src.main.scala.licenses.review

import java.io.FileNotFoundException
import java.nio.file.{Files, Path}
import java.time.LocalDate

import sbt._
import src.main.scala.licenses._

import scala.util.control.NonFatal

case class Review(root: File, dependencySummary: DependencySummary) {
  def run(): ReviewedSummary = {
    val reviews = dependencySummary.dependencies.map {
      case (information, attachments) =>
        reviewDependency(information, attachments)
    }

    val header = prepareHeader()
    val files  = findAdditionalFiles()
    ReviewedSummary(reviews, header, files)
  }

  private def prepareHeader(): String =
    readFile(root / "notice-header").getOrElse(Review.defaultHeader)

  private def findAdditionalFiles(): Seq[AttachedFile] = {
    val additionalFilesRoot = root / "files-add"
    def loadFile(file: File): AttachedFile = {
      val path = file
        .relativeTo(additionalFilesRoot)
        .map(_.toPath)
        .getOrElse(Path.of(file.getName))
      val content = IO.read(file)
      AttachedFile(path, content)
    }
    listFiles(additionalFilesRoot).map(loadFile)
  }

  private def reviewDependency(
    info: DependencyInformation,
    attachments: Seq[Attachment]
  ): ReviewedDependency = {
    val (licenseReviewed, licensePath) = reviewLicense(info)
    val (files, copyrights)            = Attachments.split(attachments)
    val copyrightsDeduplicated =
      Copyrights.removeCopyrightsIncludedInNotices(copyrights, files)
    val packageRoot = root / info.packageName

    val processedFiles =
      reviewFiles(packageRoot, files) ++ addFiles(packageRoot)
    val processedCopyrights =
      reviewCopyrights(packageRoot, copyrightsDeduplicated) ++
      addCopyrights(packageRoot)

    ReviewedDependency(
      information     = info,
      licenseReviewed = licenseReviewed,
      licensePath     = licensePath,
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
  ): (Boolean, Option[Path]) = {
    val file =
      root.getParentFile / "reviewed-licenses" /
      Review.normalizeName(info.license.name)

    if (Files.exists((root / "custom-license").toPath)) (true, None)
    else
      readFile(file)
        .map(p => (true, Some(Path.of(p.strip()))))
        .getOrElse((false, None))
  }

  private def readLines(file: File): Seq[String] =
    try { IO.readLines(file).map(_.strip) }
    catch { case _: FileNotFoundException => Seq() }

  private def readFile(file: File): Option[String] =
    try { Some(IO.read(file)) }
    catch { case NonFatal(_) => None }

  private def listFiles(dir: File): Seq[File] = {
    try { IO.listFiles(dir) }
    catch { case NonFatal(_) => Seq() }
  }
}

object Review {
  def normalizeName(string: String): String = {
    val charsToReplace = " /:;,"
    charsToReplace.foldLeft(string)((str, char) => str.replace(char, '_'))
  }

  val defaultHeader: String = {
    val yearStart   = 2020
    val yearCurrent = LocalDate.now().getYear
    val year =
      if (yearCurrent > yearStart) s"$yearStart - $yearCurrent"
      else s"$yearStart"
    s"""Enso
       |Copyright $year New Byte Order sp. z o. o.""".stripMargin
  }
}
