package src.main.scala.licenses.report

import java.nio.file.{InvalidPathException, Path}
import java.time.LocalDate
import sbt._
import src.main.scala.licenses._

import java.nio.charset.StandardCharsets
import java.util.Base64
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
  def run(): WithDiagnostics[ReviewedSummary] =
    for {
      reviews <- dependencySummary.dependencies.map {
        case (information, attachments) =>
          reviewDependency(information, attachments)
      }.flip

      header  = findHeader()
      files   = findAdditionalFiles(root / Paths.filesAdd)
      summary = ReviewedSummary(reviews, header, files)
      _ <- ReviewedSummary.warnAboutMissingReviews(summary)
      expectedPackages = dependencySummary.dependencies.map(_._1)
      _ <- warnAboutMissingDependencies(expectedPackages)
    } yield summary

  /** Returns a list of warnings for dependencies whose configuration has been
    * detected but which have not been detected.
    *
    * This may be used to detect dependencies that have been removed after an
    * update.
    */
  private def warnAboutMissingDependencies(
    knownPackages: Seq[DependencyInformation]
  ): WithDiagnostics[Unit] = {
    val foundConfigurations = listFiles(root).filter(_.isDirectory)
    val expectedFileNames =
      knownPackages.map(_.packageName) ++ Seq(
        Paths.filesAdd,
        Paths.reviewedLicenses
      )

    val unexpectedConfigurations =
      foundConfigurations.filter(p => !expectedFileNames.contains(p.getName))
    val diagnostics = unexpectedConfigurations.map { p: File =>
      val packageNameFromConfig = p.getName
      val matchingPackages = knownPackages.filter(other =>
        packageNameFromConfig.startsWith(other.packageNameWithoutVersion + "-")
      )
      val maybeMatchingPackage: Option[DependencyInformation] =
        if (matchingPackages.length == 1) Some(matchingPackages.head) else None

      maybeMatchingPackage match {
        case Some(matchingPackage) =>
          Diagnostic.Error(
            s"Found legal review configuration for package ${p.getName}, but " +
            s"no such dependency has been found. Perhaps the version was " +
            s"changed to `${matchingPackage.packageName}`?",
            metadata = Map(
              "class"     -> "rename-dependency-config",
              "data-from" -> packageNameFromConfig,
              "data-to"   -> matchingPackage.packageName
            )
          )
        case None =>
          // The configuration is not related to any known package, so we remove it
          IO.delete(p)
          Diagnostic.Warning(
            s"Found legal review configuration for package ${p.getName}, but " +
            s"no such dependency has been found. It seems that the " +
            s"dependency has been removed, so the configuration has been " +
            s"deleted. If you think this was mistake, please rely on " +
            s"version control to bring it back."
          )
      }
    }
    WithDiagnostics.justDiagnostics(diagnostics)
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
          Review.directoryMark,
          origin = Some(dir.toPath.toString)
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
  ): WithDiagnostics[ReviewedDependency] = {
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
  ): WithDiagnostics[Seq[(AttachedFile, AttachmentStatus)]] = {
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
  ): WithDiagnostics[Seq[(CopyrightMention, AttachmentStatus)]] = {
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
  ): WithDiagnostics[LicenseReview] =
    readFile(packageRoot / Paths.customLicense) match {
      case Some(content) =>
        val customFilename = content.strip()
        WithDiagnostics(LicenseReview.Custom(customFilename))
      case None =>
        val directory   = root / Paths.reviewedLicenses
        val fileName    = Review.normalizeName(info.license.name)
        var settingPath = directory / fileName
        val reviewedLicenseFiles =
          Option(directory.listFiles).getOrElse(Array())
        reviewedLicenseFiles.filter(
          _.getName.equalsIgnoreCase(fileName)
        ) match {
          case Array(settingPath) =>
            readFile(settingPath)
              .map { content =>
                if (content.isBlank) {
                  WithDiagnostics(
                    LicenseReview.NotReviewed,
                    Seq(
                      Diagnostic.Error(
                        s"License review file $settingPath is empty, but it should contain a path to a license text inside of `license-texts`."
                      )
                    )
                  )
                } else
                  try {
                    val path = Path.of(content.strip())
                    val bothDefaultAndCustom =
                      (packageRoot / Paths.defaultAndCustomLicense).exists()
                    WithDiagnostics(
                      LicenseReview.Default(
                        path,
                        allowAdditionalCustomLicenses = bothDefaultAndCustom
                      )
                    )
                  } catch {
                    case e: InvalidPathException =>
                      WithDiagnostics(
                        LicenseReview.NotReviewed,
                        Seq(
                          Diagnostic.Error(
                            s"License review file $settingPath is malformed: $e"
                          )
                        )
                      )
                  }
              }
              .getOrElse(WithDiagnostics(LicenseReview.NotReviewed))
          case Array(_, _*) =>
            WithDiagnostics(
              LicenseReview.NotReviewed,
              Seq(
                Diagnostic.Error(
                  s"Multiple copies of file $settingPath with differing case (the license names are matched case insensitively)."
                )
              )
            )
          case Array() =>
            WithDiagnostics(
              LicenseReview.NotReviewed,
              Seq(
                Diagnostic.Error(
                  s"License review file $settingPath is missing. Either review the default license or set a `custom-license` for packages that used it."
                )
              )
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
  ): WithDiagnostics[Seq[String]] = {
    val lines           = readLines(packageRoot / fileName)
    val unexpectedLines = lines.filter(l => !expectedLines.contains(l))
    val warnings = unexpectedLines.map(l =>
      Diagnostic.Error(
        s"File $fileName in ${packageRoot.getName} contains entry `$l`, but no " +
        s"such entry has been detected. Perhaps it has disappeared after an " +
        s"update? Please remove it from the file and make sure that the report " +
        s"contains all necessary elements after this change.",
        metadata = Map(
          "class"         -> "unexpected-entry-in-file",
          "data-package"  -> packageRoot.getName,
          "data-filename" -> fileName,
          "data-content" -> Base64.getEncoder.encodeToString(
            l.getBytes(StandardCharsets.UTF_8)
          )
        )
      )
    )
    WithDiagnostics(lines, warnings)
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
