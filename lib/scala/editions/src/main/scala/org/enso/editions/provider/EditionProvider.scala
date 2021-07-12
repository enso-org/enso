package org.enso.editions.provider

import org.enso.editions.Editions

import scala.util.Try

/** Interface for a provider of editions which is able to load a raw edition
  * based on its name.
  *
  * It is used when resolving parent edition references.
  */
trait EditionProvider {

  /** Tries to load an edition with the given name. */
  def findEditionForName(name: String): Try[Editions.Raw.Edition]
}
