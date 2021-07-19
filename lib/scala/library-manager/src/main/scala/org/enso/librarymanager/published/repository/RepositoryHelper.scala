package org.enso.librarymanager.published.repository

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.http.{HTTPDownload, URIBuilder}
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName
import org.enso.yaml.YamlHelper

import java.nio.file.Path
import scala.util.Failure

object RepositoryHelper {
  class LibraryAccess(
    libraryName: LibraryName,
    version: SemVer,
    libraryRoot: URIBuilder
  ) {
    def downloadManifest(): TaskProgress[LibraryManifest] = {
      val url = (libraryRoot / manifestFilename).build()
      HTTPDownload.fetchString(url).flatMap { response =>
        response.statusCode match {
          case 200 =>
            YamlHelper.parseString[LibraryManifest](response.content).toTry
          case 404 =>
            Failure(
              LibraryNotFoundException(libraryName, version, url.toString)
            )
          case _ =>
            Failure(
              new LibraryDownloadFailure("Could not download the manifest")
            )
        }
      }
    }

    private def downloadArtifact(
      artifactName: String,
      destination: Path
    ): TaskProgress[Unit] = {
      val url = (libraryRoot / artifactName).build()
      HTTPDownload.download(url, destination).map(_ => ())
    }

    def downloadLicense(destinationDirectory: Path): TaskProgress[Unit] =
      downloadArtifact(licenseFilename, destinationDirectory / licenseFilename)

    def downloadPackageConfig(destinationDirectory: Path): TaskProgress[Unit] =
      downloadArtifact(packageFileName, destinationDirectory / packageFileName)

    def downloadArchive(
      archiveName: String,
      destinationDirectory: Path
    ): TaskProgress[Unit] = downloadArtifact(archiveName, destinationDirectory)
  }

  implicit class RepositoryMethods(val repository: Repository) {
    def resolveLibraryRoot(name: LibraryName, version: SemVer): URIBuilder =
      URIBuilder
        .fromUri(repository.url)
        .addPathSegment(name.namespace)
        .addPathSegment(name.name)
        .addPathSegment(version.toString)

    def accessLibrary(name: LibraryName, version: SemVer): LibraryAccess =
      new LibraryAccess(name, version, resolveLibraryRoot(name, version))
  }

  val manifestFilename = "manifest.yaml"
  val licenseFilename  = "LICENSE.md"
  val packageFileName  = "package.yaml"
}
