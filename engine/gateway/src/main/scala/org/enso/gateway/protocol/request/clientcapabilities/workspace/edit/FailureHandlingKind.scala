package org.enso.gateway.protocol.request.clientcapabilities.workspace.edit

import io.circe.Decoder
import io.circe.generic.extras.semiauto.deriveEnumerationDecoder

/** Part of
  * [[org.enso.gateway.protocol.request.clientcapabilities.workspace.Edit]].
  */
sealed trait FailureHandlingKind
object FailureHandlingKind {

  /** Applying the workspace change is simply aborted if one of the changes
    * provided fails. All operations executed before the failing operation stay
    * executed.
    */
  case object abort extends FailureHandlingKind

  /**
    * All operations are executed transactional. That means they either all
    * succeed or no changes at all are applied to the workspace.
    */
  case object transactional extends FailureHandlingKind

  /**
    * The client tries to undo the operations already executed. But there is no
    * guarantee that this is succeeding.
    */
  case object undo extends FailureHandlingKind

  /**
    * If the workspace edit contains only textual file changes they are executed
    * transactional. If resource changes (create, rename or delete file) are
    * part of the change the failure handling strategy is abort.
    */
  case object textOnlyTransactional extends FailureHandlingKind

  implicit val failureHandlingKindDecoder: Decoder[FailureHandlingKind] =
    deriveEnumerationDecoder
}
