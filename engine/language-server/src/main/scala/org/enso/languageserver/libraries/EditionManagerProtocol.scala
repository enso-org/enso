package org.enso.languageserver.libraries

import nl.gn0s1s.bump.SemVer

object EditionManagerProtocol {
  sealed trait Request

  case class ListAvailable(update: Boolean) extends Request
  case class ListAvailableResponse(editions: Seq[String])

  case class Resolve(editionReference: EditionReference) extends Request
  case class ResolveResponse(engineVersion: SemVer)

  case object GetProjectSettings
  case class ProjectSettingsResponse(
    parentEdition: Option[String],
    preferLocalLibraries: Boolean
  )

  case class SetParentEdition(newEditionName: String) extends Request

  case class SetLocalLibrariesPreference(preferLocalLibraries: Boolean)
      extends Request

  case class ListDefinedLibraries(editionReference: EditionReference)
      extends Request
  case class ListDefinedLibrariesResult(availableLibraries: Seq[LibraryEntry])
}
