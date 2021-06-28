package org.enso.librarymanager.published

import org.enso.cli.task.ProgressReporter
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.yaml.YamlHelper

import java.nio.file.Path

class LibraryDownloader(progressReporter: ProgressReporter) {
//  def downloadLibrary(
//    name: LibraryName,
//    version: SemVer,
//    repository: Repository,
//    destinationRoot: Path
//  ): Unit = {
//    val libraryRoot = resolveLibraryRoot(name, version, repository)
//    val manifest    = downloadManifest(libraryRoot)
//    val packages    = manifest.archives
//    for (pkg <- packages) {
//      // TODO filtering
//    }
//
//    // TODO tmp directory, unpack extract
//    // TODO locking
//    // TODO progress reporting
//  }

  def downloadManifest(libraryRoot: URIBuilder): LibraryManifest = {
    val uri      = libraryRoot.addPathSegment(LibraryManifest.fileName).build()
    val request  = HTTPRequestBuilder.fromURI(uri).GET
    val response = HTTPDownload.fetchString(request).force()
    YamlHelper.parseString[LibraryManifest](response.content).toTry.get
  }

  /** Downloads a sub-package and extracts it to the given location, it will
    * merge the contents with other packages that were extracted there earlier.
    */
  def downloadAndExtractPackage(
    libraryName: LibraryName,
    libraryRoot: URIBuilder,
    packageName: String,
    destination: Path
  ): Unit = {
    val uri     = libraryRoot.addPathSegment(packageName).build()
    val request = HTTPRequestBuilder.fromURI(uri).GET
    // TODO download to temporary location, not to the final destination!
    val download = HTTPDownload.download(request, destination / packageName)
    progressReporter.trackProgress(
      s"Downloading $libraryName ($packageName).",
      download
    )
    download.force()
    // TODO WIP
  }
}
