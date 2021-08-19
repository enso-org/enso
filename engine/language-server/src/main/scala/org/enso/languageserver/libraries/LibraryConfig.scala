package org.enso.languageserver.libraries

import akka.actor.ActorRef
import org.enso.editions.updater.EditionManager
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache

/** Gathers together components needed by library-related Language Server endpoints.
  *
  * @param localLibraryManager a reference to the local library manager actor
  * @param editionReferenceResolver an instance of edition reference resolver
  * @param editionManager an instance of edition manager
  * @param localLibraryProvider an instance of local library provider
  * @param publishedLibraryCache an instance of published library cache
  */
case class LibraryConfig(
  localLibraryManager: ActorRef,
  editionReferenceResolver: EditionReferenceResolver,
  editionManager: EditionManager,
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryCache: PublishedLibraryCache
)
