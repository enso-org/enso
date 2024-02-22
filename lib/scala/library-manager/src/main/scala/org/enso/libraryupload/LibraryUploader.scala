package org.enso.libraryupload

import com.typesafe.scalalogging.Logger
import org.enso.semver.SemVer
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.distribution.FileSystem
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.archive.TarGzWriter
import org.enso.downloader.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.libraryupload.LibraryUploader.UploadFailedError
import org.enso.pkg.{Package, PackageManager}
import org.enso.yaml.YamlHelper

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try, Using}

/** Gathers functions used for uploading libraries. */
class LibraryUploader(dependencyExtractor: DependencyExtractor[File]) {
  private lazy val logger = Logger[LibraryUploader]

  /** Uploads a library to a repository.
    *
    * @param projectRoot path to the library project root
    * @param uploadUrl an URL to the upload endpoint of a library repository
    * @param authToken a token describing the authentication method to use with
    *                  the repository
    * @param progressReporter a [[ProgressReporter]] to track long running tasks
    *                         like compression and upload
    */
  def uploadLibrary(
    projectRoot: Path,
    uploadUrl: String,
    authToken: auth.Token,
    progressReporter: ProgressReporter
  ): Try[Unit] = Try {
    FileSystem.withTemporaryDirectory("enso-upload") { tmpDir =>
      val pkg = PackageManager.Default.loadPackage(projectRoot.toFile).get
      val version = SemVer.parse(pkg.getConfig().version).getOrElse {
        throw new IllegalStateException(
          s"Project version [${pkg.getConfig().version}] is not a valid semver " +
          s"string."
        )
      }
      val uri = buildUploadUri(uploadUrl, pkg.libraryName, version)

      val filesToIgnoreInArchive = Seq(
        Package.configFileName,
        LibraryManifest.filename
      )
      val archivePath = tmpDir / mainArchiveName
      val compressing =
        createMainArchive(projectRoot, filesToIgnoreInArchive, archivePath)
      progressReporter.trackProgress(
        s"Creating the [$mainArchiveName] archive.",
        compressing
      )
      compressing.force()

      updateManifest(pkg).get

      logger.info(s"Uploading library package to the server at [$uploadUrl].")
      val upload = uploadFiles(
        uri,
        authToken,
        files = Seq(
          projectRoot / Package.configFileName,
          projectRoot / LibraryManifest.filename,
          archivePath
        )
      )
      progressReporter.trackProgress(
        s"Uploading packages to [$uploadUrl].",
        upload
      )
      upload.force()

      logger.info(s"Upload complete.")
    }
  }

  /** Updates the project's manifest by computing its dependencies.
    *
    * @param pkg package of the project that is to be updated
    */
  def updateManifest(pkg: Package[File]): Try[Unit] = Try {
    val directDependencies = dependencyExtractor.findDependencies(pkg)

    val manifestPath = pkg.root.toPath / LibraryManifest.filename
    val loadedManifest =
      loadSavedManifest(manifestPath).getOrElse(LibraryManifest.empty)
    val updatedManifest = loadedManifest.copy(
      archives     = Seq(mainArchiveName),
      dependencies = directDependencies.toSeq
    )
    FileSystem.writeTextFile(manifestPath, YamlHelper.toYaml(updatedManifest))
  }

  private val mainArchiveName = "main.tgz"

  /** Creates an URL for the upload, including information identifying the
    * library version.
    */
  private def buildUploadUri(
    baseUploadUrl: String,
    libraryName: LibraryName,
    version: SemVer
  ): URI = {
    URIBuilder
      .fromUri(baseUploadUrl)
      .addQuery("namespace", libraryName.namespace)
      .addQuery("name", libraryName.name)
      .addQuery("version", version.toString)
      .build()
  }

  /** Gathers project files to create the main archive.
    *
    * For now it just filters out the files like manifest which are uploaded
    * separately. In the future this may be extended to create separate
    * sub-archives for platform specific binaries or tests.
    *
    * @param projectRoot path to the project root
    * @param rootFilesToIgnore names of files at the root that should *not* be
    *                          included in the archive
    * @param destination path at which the archive is created
    * @return
    */
  private def createMainArchive(
    projectRoot: Path,
    rootFilesToIgnore: Seq[String],
    destination: Path
  ): TaskProgress[Unit] = {
    def relativePath(file: Path): String = projectRoot.relativize(file).toString
    def shouldBeUploaded(file: Path): Boolean = {
      def isIgnored = rootFilesToIgnore.contains(relativePath(file))
      Files.isRegularFile(file) && !isIgnored
    }

    logger.trace("Gathering files to compress.")
    val filesToCompress = Using(Files.walk(projectRoot)) { filesStream =>
      filesStream.iterator().asScala.filter(shouldBeUploaded).toSeq
    }.get

    logger.info(
      s"Compressing ${filesToCompress.size} project files " +
      s"into [${destination.getFileName}]."
    )

    val compression = TarGzWriter.compress(
      archiveDestination = destination,
      files              = filesToCompress,
      basePath           = projectRoot
    )

    compression.map { _ =>
      logger.info(s"Archive [${destination.getFileName}] created.")
    }
  }

  /** Loads a manifest, if it exists. */
  private def loadSavedManifest(manifestPath: Path): Option[LibraryManifest] = {
    if (Files.exists(manifestPath)) {
      val loaded = YamlHelper.load[LibraryManifest](manifestPath).get
      Some(loaded)
    } else None
  }

  /** Uploads the provided files to the provided url, using the provided token
    * for authentication.
    */
  private def uploadFiles(
    uri: URI,
    authToken: auth.Token,
    files: Seq[Path]
  ): TaskProgress[Unit] = {
    val request = authToken
      .alterRequest(HTTPRequestBuilder.fromURI(uri))
      .postFiles(files)
      .build()
    // TODO [RW] upload progress
    val responseFut = HTTPDownload.fetchString(request)
    responseFut.flatMap { response =>
      if (response.statusCode == 200) {
        logger.debug("Server responded with 200 OK.")
        Success(())
      } else {
        // TODO [RW] we may want to have more precise error messages to handle auth errors etc. (#1773)
        val includedMessage = for {
          json    <- io.circe.parser.parse(response.content).toOption
          obj     <- json.asObject
          message <- obj("error").flatMap(_.asString)
        } yield message
        val message = includedMessage.getOrElse("Unknown error")
        val errorMessage =
          s"Upload failed: $message (Status code: ${response.statusCode})."
        logger.error(errorMessage)
        Failure(UploadFailedError(errorMessage))
      }
    }
  }
}

object LibraryUploader {
  def apply(dependencyExtractor: DependencyExtractor[File]): LibraryUploader =
    new LibraryUploader(dependencyExtractor)

  /** Indicates that the library upload has failed. */
  case class UploadFailedError(message: String)
      extends RuntimeException(message)

}
