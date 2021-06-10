package org.enso.editions.provider

import org.enso.editions.Editions.Raw.Edition

import scala.util.Try

trait EditionProvider {
  def findEditionForName(name: String): Try[Edition]
}
