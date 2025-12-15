package org.enso.librarymanager.published.repository

import org.enso.cli.task.TaskProgress
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.http.{
  HTTPDownload,
  HTTPURIBuilder,
  JarURIBuilder,
  ResourceNotFound,
  URIBuilder
}
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName
import org.enso.pkg.{Config, Package}
import org.enso.semver.SemVer
import org.enso.yaml.YamlHelper

import java.io.IOException
import java.net.JarURLConnection
import java.nio.file.{Files, Path}
import scala.util.{Failure, Using}

/** A class that manages the HTTP API of the Library Repository.
  *
  * @see docs/libraries/repositories.md#libraries-repository
  */
object RepositoryHelper {

  /** Adds extension methods to the [[Repository]] type. */
  implicit class RepositoryMethods(val repository: Repository) {

    /** Creates a [[LibraryAccess]] instance that aids with downloading data of
      * the given library.
      */
    def accessLibrary(name: LibraryName, version: SemVer): LibraryAccess = {
      val libRoot = resolveLibraryRoot(name, version)
      libRoot match {
        case httpBldr: HTTPURIBuilder =>
          new HTTPLibraryAccess(name, version, httpBldr)
        case jarBldr: JarURIBuilder =>
          new JarLibraryAccess(name, version, jarBldr)
        case _ => throw new AssertionError(s"Unsupported URI scheme: $libRoot")
      }
    }

    /** Creates a [[URIBuilder]] that points to the directory in the repository
      * corresponding to the given library.
      */
    private def resolveLibraryRoot(
      name: LibraryName,
      version: SemVer
    ): URIBuilder =
      URIBuilder
        .fromUri(repository.url)
        .addPathSegment(name.namespace)
        .addPathSegment(name.name)
        .addPathSegment(version.toString)
  }

  /** A helper class that allows to access the Library Repository to query it
    * for metadata of a specific library or download its packages.
    */
  sealed trait LibraryAccess {
    def fetchManifest():      TaskProgress[LibraryManifest]
    def fetchPackageConfig(): TaskProgress[Config]

    /** Downloads the license file.
      *
      * It will fail with `ResourceNotFound` error if the license did not exist
      * and with a more generic `HTTPException` if it failed for other reasons.
      */
    def downloadLicense(destinationDirectory: Path): TaskProgress[Unit] =
      downloadArtifact(licenseFilename, destinationDirectory / licenseFilename)

    /** Downloads the package config file. */
    def downloadPackageConfig(destinationDirectory: Path): TaskProgress[Unit] =
      downloadArtifact(packageFileName, destinationDirectory / packageFileName)

    /** Downloads a sub-archive. */
    def downloadArchive(
      archiveName: String,
      destinationDirectory: Path
    ): TaskProgress[Unit] = downloadArtifact(archiveName, destinationDirectory)

    protected def downloadArtifact(
      artifactName: String,
      destination: Path
    ): TaskProgress[Unit]

    /** Name of the attached license file. */
    private val licenseFilename = "LICENSE.md"

    /** Name of the package config file. */
    private val packageFileName: String = Package.configFileName
  }

  /** Represents access to a JAR-based (zip) remote library repository.
    */
  sealed private class JarLibraryAccess(
    libraryName: LibraryName,
    version: SemVer,
    libraryRoot: JarURIBuilder
  ) extends LibraryAccess {

    override def fetchManifest(): TaskProgress[LibraryManifest] = {
      val uri    = (libraryRoot / LibraryManifest.filename).build()
      val jarUrl = uri.toURL.openConnection().asInstanceOf[JarURLConnection]
      val res = Using(jarUrl.getInputStream) { is =>
        val manifestContent = new String(is.readAllBytes())
        YamlHelper.parseString[LibraryManifest](manifestContent).toTry
      }.recoverWith { case _: IOException =>
        Failure(LibraryNotFoundException(libraryName, version, uri.toString))
      }
      TaskProgress.fromTry(res.flatten)
    }

    override def fetchPackageConfig(): TaskProgress[Config] = {
      val url    = (libraryRoot / Package.configFileName).build()
      val jarUrl = url.toURL.openConnection().asInstanceOf[JarURLConnection]
      val res = Using(jarUrl.getInputStream) { is =>
        val cfgContent = new String(is.readAllBytes())
        YamlHelper.parseString[Config](cfgContent).toTry
      }.recoverWith { case _: IOException =>
        Failure(LibraryNotFoundException(libraryName, version, url.toString))
      }
      TaskProgress.fromTry(res.flatten)
    }

    override protected def downloadArtifact(
      artifactName: String,
      destination: Path
    ): TaskProgress[Unit] = {
      val url    = (libraryRoot / artifactName).build()
      val jarUrl = url.toURL.openConnection().asInstanceOf[JarURLConnection]
      try {
        val is    = jarUrl.getInputStream
        val bytes = is.readAllBytes()
        Files.write(destination, bytes)
        TaskProgress.runImmediately((): Unit)
      } catch {
        case _: IOException =>
          TaskProgress.immediateFailure(ResourceNotFound())
      }
    }
  }

  sealed private class HTTPLibraryAccess(
    libraryName: LibraryName,
    version: SemVer,
    libraryRoot: HTTPURIBuilder
  ) extends LibraryAccess {

    override def fetchManifest(): TaskProgress[LibraryManifest] = {
      val uri = (libraryRoot / LibraryManifest.filename).build()
      HTTPDownload.fetchString(uri).flatMap { response =>
        response.statusCode match {
          case 200 =>
            YamlHelper.parseString[LibraryManifest](response.content).toTry
          case 404 =>
            Failure(
              LibraryNotFoundException(libraryName, version, uri.toString)
            )
          case code =>
            Failure(
              new LibraryDownloadFailure(
                s"Could not download the manifest: The repository responded " +
                s"with $code status code."
              )
            )
        }
      }
    }

    override def fetchPackageConfig(): TaskProgress[Config] = {
      val url = (libraryRoot / Package.configFileName).build()
      HTTPDownload.fetchString(url).flatMap { response =>
        response.statusCode match {
          case 200 =>
            YamlHelper.parseString[Config](response.content).toTry
          case 404 =>
            Failure(
              LibraryNotFoundException(libraryName, version, url.toString)
            )
          case code =>
            Failure(
              new LibraryDownloadFailure(
                s"Could not download the package config: The repository responded " +
                s"with $code status code."
              )
            )
        }
      }
    }

    override protected def downloadArtifact(
      artifactName: String,
      destination: Path
    ): TaskProgress[Unit] = {
      val url = (libraryRoot / artifactName).build()
      HTTPDownload.download(url, destination).map(_ => ())
    }
  }
}
