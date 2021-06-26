package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.downloader.http.{HTTPDownload, HTTPRequestBuilder, URIBuilder}
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.yaml.YamlHelper

import java.nio.file.Path

object LibraryDownloader {
  // TODO [RW]how do we report progress?
  // we could do as described below (but it is rather complex and hard to implement) or just use the ProgressReporter interface and report progress of each sub-package separately - I think this will be much better as it is much easier to implement and in the future we can modify that if need be
  // but apart from List[TaskProgress[A]] => TaskProgress[List[A]] we will also need TaskProgress[A] => (A => TaskProgress[B]) => TaskProgress[B]
  // I need to discuss how progress reporting should be handled - do we need to combine this all into a single taskprogress instance? (really complicated and will have uneven percentages) or maybe we could change the interface altogether and rely on something like
  def downloadLibrary(
    name: LibraryName,
    version: SemVer,
    repository: Repository,
    destinationRoot: Path
  ): Unit = {
    val libraryRoot = resolveLibraryRoot(name, version, repository)
    val manifest    = downloadManifest(libraryRoot)
    val packages    = manifest.archives
    for (pkg <- packages) {
      // TODO filtering
    }

    // TODO tmp directory, unpack extract
    // TODO locking
    // TODO progress reporting
  }

  private def resolveLibraryRoot(
    name: LibraryName,
    version: SemVer,
    repository: Repository
  ): URIBuilder = URIBuilder
    .fromUri(repository.url)
    .addPathSegment(name.namespace)
    .addPathSegment(name.name)
    .addPathSegment(version.toString)

  private def downloadManifest(libraryRoot: URIBuilder): LibraryManifest = {
    val uri      = libraryRoot.addPathSegment(LibraryManifest.fileName).build()
    val request  = HTTPRequestBuilder.fromURI(uri).GET
    val response = HTTPDownload.fetchString(request).force()
    YamlHelper.parseString[LibraryManifest](response.content).toTry.get
  }
}
