package org.enso.projectmanager.model

import io.circe.{Decoder, Encoder}

/** Enum that distinguishes between different kinds of projects.
  */
object ProjectKinds extends Enumeration {
  type ProjectKind = Value

  /** Enum value for user projects.
    */
  val UserProject = Value

  implicit val genderDecoder: Decoder[ProjectKind] =
    Decoder.decodeEnumeration(ProjectKinds)
  implicit val genderEncoder: Encoder[ProjectKind] =
    Encoder.encodeEnumeration(ProjectKinds)

}
