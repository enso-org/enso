package org.enso.editions

import Editions.{RawEdition, ResolvedEdition}

import scala.util.Try

case class EditionResolver(provider: EditionProvider) {
  def resolve(edition: RawEdition): Try[ResolvedEdition] = ???
}
