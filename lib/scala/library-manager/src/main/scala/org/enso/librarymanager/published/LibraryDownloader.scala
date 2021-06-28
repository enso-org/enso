package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.downloader.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.editions.Editions.Repository
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

  def downloadPac
}
