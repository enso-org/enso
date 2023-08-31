package org.enso.compiler.core.ir

import org.enso.syntax.text.Location

import java.util.UUID

/** Couples a location with a possible source identifier.
  *
  * @param location the code location.
  * @param id       the identifier for the location.
  */
case class IdentifiedLocation(location: Location, id: Option[UUID]) {

  /** @return the character index of the start of this source location.
    */
  def start: Int = location.start

  /** @return the character index of the end of this source location.
    */
  def end: Int = location.end

  /** @return the length in characters of this location.
    */
  def length: Int = location.length
}

object IdentifiedLocation {

  /** Utility constructor, building a location without an ID.
    *
    * @param location the code location.
    * @return an [[IdentifiedLocation]] corresponding to the input location.
    */
  def apply(location: Location): IdentifiedLocation =
    IdentifiedLocation(location, None)
}
