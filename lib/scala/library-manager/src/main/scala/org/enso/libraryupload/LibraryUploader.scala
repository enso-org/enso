package org.enso.libraryupload

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.stream.scaladsl.Source
import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.{
  ProgressReporter,
  TaskProgress,
  TaskProgressImplementation
}
import org.enso.distribution.FileSystem
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.archive.TarGzWriter
import org.enso.downloader.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.pkg.{Package, PackageManager}
import org.enso.yaml.YamlHelper

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try, Using}

object LibraryUploader {
  private lazy val logger = Logger[LibraryUploader.type]

  def uploadLibrary(
    projectRoot: Path,
    uploadUrl: String,
    authToken: auth.Token,
    progressReporter: ProgressReporter
  )(implicit ec: ExecutionContext): Try[Unit] = Try {
    FileSystem.withTemporaryDirectory("enso-upload") { tmpDir =>
      val pkg = PackageManager.Default.loadPackage(projectRoot.toFile).get
      val version = SemVer(pkg.config.version).getOrElse {
        throw new IllegalStateException(
          s"Project version [${pkg.config.version}] is not a valid semver " +
          s"string."
        )
      }
      val uri = buildUploadUri(uploadUrl, pkg.libraryName, version)

      val mainArchiveName = "main.tgz"
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

      val manifestPath = projectRoot / LibraryManifest.filename
      val loadedManifest =
        loadSavedManifest(manifestPath).getOrElse(LibraryManifest.empty)
      val updatedManifest =
        // TODO [RW] update dependencies in the manifest
        loadedManifest.copy(archives = Seq(mainArchiveName))
      FileSystem.writeTextFile(manifestPath, YamlHelper.toYaml(updatedManifest))

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

  private def buildUploadUri(
    baseUploadUrl: String,
    libraryName: LibraryName,
    version: SemVer
  ): Uri = {
    // TODO [RW] decide on the API
    URIBuilder
      .fromUri(baseUploadUrl)
      .addQuery("namespace", libraryName.namespace)
      .addQuery("name", libraryName.name)
      .addQuery("version", version.toString)
      .build()
  }

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

    val sumSize = filesToCompress.map(Files.size).sum

    logger.info(
      s"Compressing ${filesToCompress.size} project files " +
      s"into [${destination.getFileName}]."
    )

    val taskProgress = new TaskProgressImplementation[Unit]()

    def runCompresion(): Unit = {
      val result = TarGzWriter.createArchive(destination) { writer =>
        var totalBytesWritten: Long = 0
        def update(): Unit =
          taskProgress.reportProgress(totalBytesWritten, Some(sumSize))
        update()
        for (file <- filesToCompress) {
          // TODO [RW] Ideally we could report progress for each chunk, offering
          //  more granular feedback for big data files.
          val bytesWritten = writer.writeFile(relativePath(file), file)
          totalBytesWritten += bytesWritten
          update()
        }
      }

      logger.info(s"Archive [${destination.getFileName}] created.")
      taskProgress.setComplete(result)
    }

    val thread = new Thread(() => runCompresion(), "Writing-Archive")
    thread.start()

    taskProgress
  }

  private def createRequestEntity(
    files: Seq[Path]
  )(implicit ec: ExecutionContext): Future[RequestEntity] = {

    val fileBodies = files.map { path =>
      val filename = path.getFileName.toString
      Multipart.FormData.BodyPart(
        filename,
        HttpEntity.fromPath(detectContentType(path), path),
        Map("filename" -> filename)
      )
    }

    val formData = Multipart.FormData(Source(fileBodies))
    Marshal(formData).to[RequestEntity]
  }

  private def loadSavedManifest(manifestPath: Path): Option[LibraryManifest] = {
    if (Files.exists(manifestPath)) {
      val loaded = YamlHelper.load[LibraryManifest](manifestPath).get
      Some(loaded)
    } else None
  }

  private def detectContentType(path: Path): ContentType = {
    val filename = path.getFileName.toString
    if (filename.endsWith(".tgz") || filename.endsWith(".tar.gz"))
      ContentType(MediaTypes.`application/x-gtar`)
    else if (filename.endsWith(".yaml") || filename.endsWith(".enso"))
      ContentTypes.`text/plain(UTF-8)`
    else ContentTypes.`application/octet-stream`
  }

  private def uploadFiles(
    uri: Uri,
    authToken: auth.Token,
    files: Seq[Path]
  )(implicit ec: ExecutionContext): TaskProgress[Unit] = {
    val future = createRequestEntity(files).map { entity =>
      val request = authToken
        .alterRequest(HTTPRequestBuilder.fromURI(uri))
        .setEntity(entity)
        .POST
      // TODO [RW] upload progress
      HTTPDownload.fetchString(request).force()
    }
    TaskProgress.fromFuture(future).flatMap { response =>
      if (response.statusCode == 200) {
        logger.debug("Server responded with 200 OK.")
        Success(())
      } else {
        // TODO we may want to have more precise error messages to handle auth errors etc.
        val includedMessage = for {
          json    <- io.circe.parser.parse(response.content).toOption
          obj     <- json.asObject
          message <- obj("error").flatMap(_.asString)
        } yield message
        val message = includedMessage.getOrElse("Unknown error")
        val errorMessage =
          s"Upload failed: $message (Status code: ${response.statusCode})."
        logger.error(errorMessage)
        Failure(
          new RuntimeException( // TODO more precise exceptions
            errorMessage
          )
        )
      }
    }
  }
}
