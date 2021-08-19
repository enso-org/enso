package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.editions.LibraryVersion
import org.enso.jsonrpc.{Request, ResponseError, ResponseResult}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries.{
  EditionReferenceResolver,
  LibraryEntry
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache

import scala.util.{Failure, Success}

/** A request handler for the `editions/listDefinedLibraries` endpoint.
  *
  * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
  * @param localLibraryProvider     a provider of local libraries
  * @param publishedLibraryCache    a cache of published libraries
  */
class EditionsListDefinedLibrariesHandler(
  editionReferenceResolver: EditionReferenceResolver,
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryCache: PublishedLibraryCache
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = {
    case Request(
          EditionsListDefinedLibraries,
          id,
          EditionsListDefinedLibraries.Params(reference)
        ) =>
      val result = for {
        edition <- editionReferenceResolver.resolveEdition(reference)
      } yield edition.getAllDefinedLibraries.toSeq.map { case (name, version) =>
        val isCached = version match {
          case LibraryVersion.Local =>
            localLibraryProvider.findLibrary(name).isDefined
          case LibraryVersion.Published(version, _) =>
            publishedLibraryCache.isLibraryCached(name, version)
        }
        LibraryEntry(
          namespace = name.namespace,
          name      = name.name,
          version   = version,
          isCached  = isCached
        )
      }

      result match {
        case Success(libraries) =>
          sender() ! ResponseResult(
            EditionsListDefinedLibraries,
            id,
            EditionsListDefinedLibraries.Result(libraries)
          )

        case Failure(exception) =>
          // TODO [RW] more detailed errors
          sender() ! ResponseError(
            Some(id),
            FileSystemError(exception.getMessage)
          )
      }
  }
}

object EditionsListDefinedLibrariesHandler {

  /** Creates a configuration object to create
    * [[EditionsListDefinedLibrariesHandler]].
    *
    * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
    * @param localLibraryProvider     a provider of local libraries
    * @param publishedLibraryCache    a cache of published libraries
    */
  def props(
    editionReferenceResolver: EditionReferenceResolver,
    localLibraryProvider: LocalLibraryProvider,
    publishedLibraryCache: PublishedLibraryCache
  ): Props = Props(
    new EditionsListDefinedLibrariesHandler(
      editionReferenceResolver,
      localLibraryProvider,
      publishedLibraryCache
    )
  )
}
