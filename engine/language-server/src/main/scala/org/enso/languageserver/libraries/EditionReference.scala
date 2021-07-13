package org.enso.languageserver.libraries

sealed trait EditionReference
object EditionReference {
  case class NamedEdition(editionName: String) extends EditionReference
  case object CurrentProjectEdition            extends EditionReference
}
