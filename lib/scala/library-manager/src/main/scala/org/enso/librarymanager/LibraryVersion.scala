package org.enso.librarymanager

import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.Repository

sealed trait LibraryVersion
object LibraryVersion {
  case object Local extends LibraryVersion
  case class Published(version: SemVer, repository: Repository)
      extends LibraryVersion
}
