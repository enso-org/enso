package org.enso.libraryupload

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.stream.scaladsl.Source
import nl.gn0s1s.bump.SemVer
import org.enso.distribution.FileSystem
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.pkg.{Package, PackageManager}
import org.enso.yaml.YamlHelper

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object LibraryUploader {
  def uploadLibrary(
    projectRoot: Path,
    uploadUrl: String,
    authToken: auth.Token
  )(implicit ec: ExecutionContext): Try[Unit] = Try {
    // TODO create main.tgz package of all files apart from package.yaml which is separately uploaded

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
      createMainArchive(
        projectRoot,
        filesToIgnoreInArchive,
        tmpDir / mainArchiveName
      )

      val manifestPath = projectRoot / LibraryManifest.filename
      val loadedManifest =
        loadSavedManifest(manifestPath).getOrElse(LibraryManifest.empty)
      val updatedManifest =
        // TODO update dependencies in the manifest
        loadedManifest.copy(archives = Seq(mainArchiveName))
      FileSystem.writeTextFile(manifestPath, YamlHelper.toYaml(updatedManifest))

      // TODO add archive file to upload
      uploadFiles(
        uri,
        authToken,
        files = Seq(
          projectRoot / Package.configFileName,
          projectRoot / LibraryManifest.filename
        )
      )
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
      .addPathSegment(libraryName.namespace)
      .addPathSegment(libraryName.name)
      .addPathSegment(version.toString)
      .build()
  }

  private def createMainArchive(
    projectRoot: Path,
    rootFilesToIgnore: Seq[String],
    destination: Path
  ): Unit = {
    val _ = (projectRoot, rootFilesToIgnore, destination)
    // TODO
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
  )(implicit ec: ExecutionContext): Future[Unit] = {
    createRequestEntity(files).map { entity =>
      val request = authToken
        .alterRequest(HTTPRequestBuilder.fromURI(uri))
        .setEntity(entity)
        .POST
      // TODO upload progress will require a separate mechanism
      HTTPDownload.fetchString(request).force()
    }
  }
}
