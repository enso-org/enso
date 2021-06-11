package org.enso.editions

import nl.gn0s1s.bump.SemVer

import java.net.URL
import scala.util.Try

trait Editions {
  type NestedEditionType
  type LibraryRepositoryType
  type PreferLocalLibrariesSettingType

  sealed trait Library {
    def qualifiedName: String
  }
  case class LocalLibrary(override val qualifiedName: String) extends Library
  case class PublishedLibrary(
    override val qualifiedName: String,
    version: SemVer,
    repository: LibraryRepositoryType
  ) extends Library

  case class Edition(
    parent: Option[NestedEditionType],
    engineVersion: Option[EnsoVersion],
    preferLocalLibraries: PreferLocalLibrariesSettingType,
    repositories: Map[String, Editions.Repository],
    libraries: Map[String, Library]
  )
}

object Editions {
  case class Repository(name: String, url: URL)

  object Repository {
    def make(name: String, url: String): Try[Repository] = Try {
      Repository(name, new URL(url))
    }
  }

  object Raw extends Editions {
    override type NestedEditionType               = String
    override type LibraryRepositoryType           = String
    override type PreferLocalLibrariesSettingType = Option[Boolean]
  }

  object Resolved extends Editions {
    override type NestedEditionType               = this.Edition
    override type LibraryRepositoryType           = Repository
    override type PreferLocalLibrariesSettingType = Boolean
  }

  type RawEdition      = Raw.Edition
  type ResolvedEdition = Resolved.Edition
  implicit class ResolvedEditionOps(edition: ResolvedEdition) {
    def getEngineVersion: EnsoVersion = edition.engineVersion.getOrElse {
      val parent = edition.parent.getOrElse {
        throw new IllegalStateException(
          "Internal error: Resolved edition does not imply an engine version."
        )
      }
      parent.getEngineVersion
    }
  }
}
