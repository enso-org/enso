package org.enso.languageserver.libraries

import akka.actor.ActorRef
import org.enso.editions.updater.EditionManager
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache

case class LibraryConfig(
  localLibraryManager: ActorRef,
  editionReferenceResolver: EditionReferenceResolver,
  editionManager: EditionManager,
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryCache: PublishedLibraryCache
)
