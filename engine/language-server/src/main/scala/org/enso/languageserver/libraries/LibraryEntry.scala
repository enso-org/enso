package org.enso.languageserver.libraries

case class LibraryEntry(
  namespace: String,
  name: String,
  version: LibraryEntry.LibraryVersion
)

object LibraryEntry {
  // TODO [RW] proper case serialization
  sealed trait LibraryVersion
  case object LocalLibraryVersion extends LibraryVersion
  case class PublishedLibraryVersion(version: String, repositoryUrl: String)
}
