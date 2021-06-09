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
  case class RegularLibrary(
    override val qualifiedName: String,
    version: SemVer,
    repository: LibraryRepositoryType
  ) extends Library

  case class Edition(
    parent: Option[NestedEditionType],
    engineVersion: Option[SemVer],
    preferLocalLibraries: PreferLocalLibrariesSettingType,
    repositories: Map[String, Editions.Repository],
    libraries: Map[String, Library]
  )
}

object Editions {
  case class Repository(name: String, url: URL)

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
    def getEngineVersion: SemVer = edition.engineVersion.getOrElse {
      val parent = edition.parent.getOrElse {
        throw new IllegalStateException(
          "Internal error: Resolved edition does not imply an engine version."
        )
      }
      parent.getEngineVersion
    }
  }
}
