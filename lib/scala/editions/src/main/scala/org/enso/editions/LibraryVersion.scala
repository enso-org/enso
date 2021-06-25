package org.enso.editions

import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.Repository

/** A resolved version of the library. */
sealed trait LibraryVersion
object LibraryVersion {

  /** Indicates that the version from the local library path should be used. */
  case object Local extends LibraryVersion {

    /** @inheritdoc */
    override def toString: String = "local"
  }

  /** Indicates that a particular published version should be used.
    *
    * It also indicates the repository to download the library from if it is not
    * already cached.
    */
  case class Published(version: SemVer, repository: Repository)
      extends LibraryVersion {

    /** @inheritdoc */
    override def toString: String = version.toString
  }
}
