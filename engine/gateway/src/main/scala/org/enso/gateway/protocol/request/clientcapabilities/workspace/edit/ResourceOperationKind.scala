package org.enso.gateway.protocol.request.clientcapabilities.workspace.edit

import io.circe.Decoder
import io.circe.generic.extras.semiauto.deriveEnumerationDecoder

/** Part of
  * [[org.enso.gateway.protocol.request.clientcapabilities.workspace.Edit]].
  */
sealed trait ResourceOperationKind
object ResourceOperationKind {

  /** Supports creating new files and folders. */
  case object create extends ResourceOperationKind

  /** Supports renaming existing files and folders. */
  case object rename extends ResourceOperationKind

  /** Supports deleting existing files and folders. */
  case object delete extends ResourceOperationKind

  implicit val resourceOperationKindDecoder: Decoder[ResourceOperationKind] =
    deriveEnumerationDecoder
}
