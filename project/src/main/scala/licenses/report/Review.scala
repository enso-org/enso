package src.main.scala.licenses.report

import java.nio.file.{InvalidPathException, Path}
import java.time.LocalDate

import sbt._
import src.main.scala.licenses._

import scala.util.control.NonFatal

/** Reads settings from the `root` to add review statuses to discovered
  * attachments and add any additional attachments coming from the settings.
  *
  * The review settings consist of the following files or directories (all are
  * optional):
  *
  * - `notice-header` - contains the header that will start the main generated
  *   NOTICE
  * - `files-add` - directory that may contain additional files that should be
  *   added to the package
  * - `reviewed-licenses` - directory that may contain files for reviewed
  *   licenses; the files should be named with the normalized license name and
  *   they should contain a path to that license's file
  * - and for each dependency, a subdirectory named as its `packageName` with
  *   following entries:
  *    - `files-add` - directory that may contain additional files that should
  *      be added to the subdirectory for this package
  *    - `files-keep` - a file containing names of files found in the package
  *      sources that should be included in the package
  *    - `files-ignore` - a file containing names of files found in the package
  *      sources that should not be included
  *    - `custom-license` - a file that indicates that the dependency should not
  *      point to the default license, but it should contain a custom one within
  *      its files
  *   - `default-and-custom-license` - a file that indicates that the dependency should point to the
  *     default license, but it also contains additional license-like files that should be kept too;
  *     it disables checking if the attached license-like files are equal to the default license or
  *     not, so it should be used very carefully; at most one of `default-and-custom-license` and
  *     `custom-license` should exist for each dependency
  *    - `copyright-keep` - copyright lines that should be included in the
  *      notice summary for the package
  *    - `copyright-keep-context` - copyright lines that should be included
  *      (alongside with their context) in the notice summary for the package
  *    - `copyright-ignore` - copyright lines that should not be included in the
  *      notice summary for the package
  *    - `copyright-add` - a single file whose contents will be added to the
  *      notice summary for the package
  *
  * @param root directory that contains files which guide the review process
  * @param dependencySummary summary of discovered dependency information
  */
case class Review(root: File, dependencySummary: DependencySummary) {

  private object Paths {
    val noticeHeader = "notice-header"

    val filesAdd    = "files-add"
    val filesKeep   = "files-keep"
    val filesIgnore = "files-ignore"

    val copyrightKeep            = "copyright-keep"
    val copyrightKeepWithContext = "copyright-keep-context"
    val copyrightIgnore          = "copyright-ignore"
    val copyrightAdd             = "copyright-add"

    val reviewedLicenses        = "reviewed-licenses"
    val customLicense           = "custom-license"
    val defaultAndCustomLicense = "default-and-custom-license"
  }

  /** Runs the review process, returning a [[ReviewedDependency]] which includes
    * information from the [[DependencySummary]] enriched with review statuses.
    */
  def run(): WithWarnings[ReviewedSummary] =
    for {
      reviews <- dependencySummary.dependencies.map {
        case (information, attachments) =>
          reviewDependency(information, attachments)
      }.flip

      header  = findHeader()
      files   = findAdditionalFiles(root / Paths.filesAdd)
      summary = ReviewedSummary(reviews, header, files)
      _ <- ReviewedSummary.warnAboutMissingReviews(summary)
      existingPackages = dependencySummary.dependencies.map(_._1.packageName)
      _ <- warnAboutMissingDependencies(existingPackages)
    } yield summary

  /** Returns a list of warnings for dependencies whose configuration has been
    * detected but which have not been detected.
    *
    * This may be used to detect dependencies that have been removed after an
    * update.
    */
  private def warnAboutMissingDependencies(
    existingPackageNames: Seq[String]
  ): WithWarnings[Unit] = {
    val foundConfigurations = listFiles(root).filter(_.isDirectory)
    val expectedFileNames =
      existingPackageNames ++ Seq(Paths.filesAdd, Paths.reviewedLicenses)
    val unexpectedConfigurations =
      foundConfigurations.filter(p => !expectedFileNames.contains(p.getName))
    val warnings = unexpectedConfigurations.map(p =>
      s"Found legal review configuration for package ${p.getName}, " +
      s"but no such dependency has been found. Perhaps it has been removed?"
    )
    WithWarnings.justWarnings(warnings)
  }

  /** Finds a header defined in the settings or
    */
  private def findHeader(): String =
    readFile(root / Paths.noticeHeader).getOrElse(Review.defaultHeader)

  /** Reads files from the provided directory as [[AttachedFile]].
    */
  private def findAdditionalFiles(dir: File): Seq[AttachedFile] =
    listFiles(dir).map { f =>
      if (f.isDirectory)
        AttachedFile(
          PortablePath(f.toPath.toAbsolutePath),
          Review.directoryMark
        )
      else
        AttachedFile.read(f.toPath, Some(dir.toPath))
    }

  /** Splits the sequence of attachments into sequences of files and copyrights.
    */
  private def splitAttachments(
    attachments: Seq[Attachment]
  ): (Seq[AttachedFile], Seq[CopyrightMention]) = {
    val notices    = attachments.collect { case n: AttachedFile => n }
    val copyrights = attachments.collect { case c: CopyrightMention => c }
    (notices, copyrights)
  }

  /** Returns only such copyrights that are not included in one of the
    * discovered files.
    */
  private def removeCopyrightsIncludedInNotices(
    copyrights: Seq[CopyrightMention],
    notices: Seq[AttachedFile]
  ): Seq[CopyrightMention] = {
    def shouldKeepCopyright(copyright: CopyrightMention): Boolean = {
      val allOriginsAreFresh = copyright.origins.forall { path =>
        !notices.exists(_.path == path)
      }
      allOriginsAreFresh
    }
    copyrights.filter(shouldKeepCopyright)
  }

  /** Returns the review status of the license and any attachments associated
    * with the dependency.
    */
  private def reviewDependency(
    info: DependencyInformation,
    attachments: Seq[Attachment]
  ): WithWarnings[ReviewedDependency] = {
    val packageRoot         = root / info.packageName
    val (files, copyrights) = splitAttachments(attachments)
    val copyrightsDeduplicated =
      removeCopyrightsIncludedInNotices(copyrights, files)

    for {
      licenseReview  <- reviewLicense(packageRoot, info)
      processedFiles <- reviewFiles(packageRoot, files) ++ addFiles(packageRoot)
      processedCopyrights <-
        reviewCopyrights(packageRoot, copyrightsDeduplicated) ++
        addCopyrights(packageRoot)
    } yield ReviewedDependency(
      information   = info,
      licenseReview = licenseReview,
      files         = processedFiles,
      copyrights    = processedCopyrights
    )
  }

  /** Enriches the file attachments with their review status.
    */
  private def reviewFiles(
    packageRoot: File,
    files: Seq[AttachedFile]
  ): WithWarnings[Seq[(AttachedFile, AttachmentStatus)]] = {
    def keyForFile(file: AttachedFile): String = file.path.toString
    val keys                                   = files.map(keyForFile)
    for {
      ignore <- readExpectedLines(Paths.filesIgnore, keys, packageRoot)
      keep   <- readExpectedLines(Paths.filesKeep, keys, packageRoot)
    } yield {
      def review(file: AttachedFile): AttachmentStatus = {
        val key = keyForFile(file)
        if (keep.contains(key)) AttachmentStatus.Keep
        else if (ignore.contains(key)) AttachmentStatus.Ignore
        else AttachmentStatus.NotReviewed
      }

      files.map(f => (f, review(f)))
    }
  }

  /** Returns any additional file attachments that are manually added in the
    * review.
    */
  private def addFiles(
    packageRoot: File
  ): Seq[(AttachedFile, AttachmentStatus)] =
    findAdditionalFiles(packageRoot / Paths.filesAdd)
      .map((_, AttachmentStatus.Added))

  /** Enriches the copyright attachments with their review status.
    */
  private def reviewCopyrights(
    packageRoot: File,
    copyrights: Seq[CopyrightMention]
  ): WithWarnings[Seq[(CopyrightMention, AttachmentStatus)]] = {
    def keyForMention(copyrightMention: CopyrightMention): String =
      copyrightMention.content.strip
    val keys = copyrights.map(keyForMention)
    for {
      ignore <- readExpectedLines(Paths.copyrightIgnore, keys, packageRoot)
      keep   <- readExpectedLines(Paths.copyrightKeep, keys, packageRoot)
      keepContext <-
        readExpectedLines(Paths.copyrightKeepWithContext, keys, packageRoot)
    } yield {

      def review(copyright: CopyrightMention): AttachmentStatus = {
        val key = keyForMention(copyright)
        if (keepContext.contains(key)) AttachmentStatus.KeepWithContext
        else if (keep.contains(key)) AttachmentStatus.Keep
        else if (ignore.contains(key)) AttachmentStatus.Ignore
        else AttachmentStatus.NotReviewed
      }

      copyrights.map(c => (c, review(c)))
    }
  }

  /** Returns any additional copyright attachments that are manually added in
    * the review.
    */
  private def addCopyrights(
    packageRoot: File
  ): Seq[(CopyrightMention, AttachmentStatus)] =
    readFile(packageRoot / Paths.copyrightAdd)
      .map(text =>
        (
          CopyrightMention(
            content  = "<manually added mentions>",
            contexts = Seq(text)
          ),
          AttachmentStatus.Added
        )
      )
      .toSeq

  /** Checks review status of the license associated with the given dependency.
    */
  private def reviewLicense(
    packageRoot: File,
    info: DependencyInformation
  ): WithWarnings[LicenseReview] =
    readFile(packageRoot / Paths.customLicense) match {
      case Some(content) =>
        val customFilename = content.strip()
        WithWarnings(LicenseReview.Custom(customFilename))
      case None =>
        val directory   = root / Paths.reviewedLicenses
        val fileName    = Review.normalizeName(info.license.name)
        var settingPath = directory / fileName
        directory.listFiles.filter(_.getName.equalsIgnoreCase(fileName)) match {
          case Array(settingPath) =>
            readFile(settingPath)
              .map { content =>
                if (content.isBlank) {
                  WithWarnings(
                    LicenseReview.NotReviewed,
                    Seq(s"License review file $settingPath is empty.")
                  )
                } else
                  try {
                    val path = Path.of(content.strip())
                    val bothDefaultAndCustom =
                      (packageRoot / Paths.defaultAndCustomLicense).exists()
                    WithWarnings(
                      LicenseReview.Default(
                        path,
                        allowAdditionalCustomLicenses = bothDefaultAndCustom
                      )
                    )
                  } catch {
                    case e: InvalidPathException =>
                      WithWarnings(
                        LicenseReview.NotReviewed,
                        Seq(
                          s"License review file $settingPath is malformed: $e"
                        )
                      )
                  }
              }
              .getOrElse(WithWarnings(LicenseReview.NotReviewed))
          case Array(_, _*) =>
            WithWarnings(
              LicenseReview.NotReviewed,
              Seq(s"Multiple copies of file $settingPath with differing case.")
            )
          case Array() =>
            WithWarnings(
              LicenseReview.NotReviewed,
              Seq(s"License review file $settingPath is missing.")
            )
        }
    }

  /** Reads the file as lines.
    *
    * Returns an empty sequence if the file cannot be read.
    */
  private def readLines(file: File): Seq[String] =
    try { IO.readLines(file).map(_.strip).filter(_.nonEmpty) }
    catch { case NonFatal(_) => Seq() }

  /** Reads the file as lines and reports any lines that were not expected to be
    * found.
    */
  private def readExpectedLines(
    fileName: String,
    expectedLines: Seq[String],
    packageRoot: File
  ): WithWarnings[Seq[String]] = {
    val lines           = readLines(packageRoot / fileName)
    val unexpectedLines = lines.filter(l => !expectedLines.contains(l))
    val warnings = unexpectedLines.map(l =>
      s"File $fileName in ${packageRoot.getName} contains entry `$l`, but no " +
      s"such entry has been detected. Perhaps it has disappeared after an " +
      s"update? Please remove it from the file and make sure that the report " +
      s"contains all necessary elements after this change."
    )
    WithWarnings(lines, warnings)
  }

  /** Reads the file as a [[String]].
    *
    * Returns None if the file cannot be read.
    */
  private def readFile(file: File): Option[String] =
    try { Some(IO.read(file)) }
    catch { case NonFatal(_) => None }

  /** Returns a sequence of files contained in a directory.
    *
    * If the directory does not exist or otherwise cannot be queried, returns an
    * empty sequence.
    */
  private def listFiles(dir: File): Seq[File] = {
    try { IO.listFiles(dir) }
    catch { case NonFatal(_) => Seq() }
  }
}

object Review {

  /** Normalizes a name so that it can be used as a filename.
    */
  def normalizeName(string: String): String = {
    val charsToReplace = " /:;,"
    charsToReplace.foldLeft(string)((str, char) => str.replace(char, '_'))
  }

  /** Default NOTICE header.
    */
  val defaultHeader: String = {
    val yearStart   = 2020
    val yearCurrent = LocalDate.now().getYear
    val year =
      if (yearCurrent > yearStart) s"$yearStart - $yearCurrent"
      else s"$yearStart"
    s"""Enso
       |Copyright $year New Byte Order sp. z o. o.""".stripMargin
  }

  val directoryMark = "<a directory>"
}
