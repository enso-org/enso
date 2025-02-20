package org.enso.projectmanager.data

import io.circe.{Decoder, Encoder}

/** Specifies how to handle missing components. */
object MissingComponentActions extends Enumeration {
  type MissingComponentAction = Value

  /** Fail - specifies that an action requiring a missing component should fail.
    * Install - specifies that an action requiring a missing component should install it,
    * unless it is broken.
    * ForceInstallBroken - specifies that an action requiring a missing component should forcibly
    * install it, even if it is broken.
    */
  val Fail, Install, ForceInstallBroken = Value

  implicit val genderDecoder: Decoder[MissingComponentAction] =
    Decoder.decodeEnumeration(MissingComponentActions)
  implicit val genderEncoder: Encoder[MissingComponentAction] =
    Encoder.encodeEnumeration(MissingComponentActions)
}
