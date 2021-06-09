package org.enso.editions

import scala.util.Try

trait EditionProvider {
  def findEditionForName(name: String): Try[Editions.Raw.Edition]
}
