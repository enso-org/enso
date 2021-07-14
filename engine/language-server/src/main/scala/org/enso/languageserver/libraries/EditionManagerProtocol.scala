package org.enso.languageserver.libraries

import nl.gn0s1s.bump.SemVer

object EditionManagerProtocol {
  sealed trait Request

  case class ListAvailable(update: Boolean) extends Request
  case class ListAvailableResponse(editions: Seq[String])

  case class Resolve(editionReference: EditionReference)
  case class ResolveResponse(engineVersion: SemVer)
}
