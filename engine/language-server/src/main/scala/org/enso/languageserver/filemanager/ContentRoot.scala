package org.enso.languageserver.filemanager

import enumeratum._

import java.io.File
import java.util.UUID

/** A representation of a content root.
  *
  * @param id the unique identifier of the content root
  * @param type the type of the content root
  * @param name The name of the content root
  */
case class ContentRoot(id: UUID, `type`: ContentRootType, name: String)

/** The type of entity that the content root represents.
  */
sealed trait ContentRootType extends EnumEntry
object ContentRootType extends Enum[ContentRootType] with CirceEnum[ContentRootType] {

  /** The content root represents the root of the current Enso project.
    */
  case object Project extends ContentRootType

  /** The content root represents a system root (`/` on unix, drives on
    * windows).
    *
    * There may be multiple of this type of root sent by default.
    */
  case object Root extends ContentRootType

  /** The content root represents the user's home directory.
    */
  case object Home extends ContentRootType

  /** The content root represents an Enso library.
    */
  case object Library extends ContentRootType

  /** The content root was a custom location added by the IDE.
    */
  case object Custom extends ContentRootType

  /** Necessary for Enumeratum and Circe. */
  override val values = findValues
}

/** A representation of a content root.
  *
  * @param id the unique identifier of the content root
  * @param `type` the type of the content root
  * @param name The name of the content root
  * @param file the file on the filesystem that is the content root
  */
case class ContentRootWithFile(
  id: UUID,
  `type`: ContentRootType,
  name: String,
  file: File
) {

  /** Convert this to a content root for use in the protocol.
    *
    * @return a protocol content root
    */
  def toContentRoot: ContentRoot = {
    ContentRoot(id, `type`, name)
  }
}
