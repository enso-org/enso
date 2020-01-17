package org.enso.gateway.protocol.request

import io.circe.generic.semiauto._
import io.circe.Decoder
import cats.syntax.functor._
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder
import org.enso.gateway.protocol.request.Param.{
  ClientCapabilities,
  ClientInfo,
  InitializationOptions,
  Trace,
  WorkspaceFolder
}

/** Params of [[org.enso.gateway.protocol.RequestOrNotification]].
  * Can be array or JSON object.
  */
sealed trait Params
object Params {
  implicit val paramsDecoder: Decoder[Params] = List[Decoder[Params]](
    Decoder[InitializeParams].widen,
    Decoder[InitializedParams].widen,
    Decoder[Array].widen
  ).reduceLeft(_ or _)

  type DocumentUri = String

  /** Params of the request
    * [[org.enso.gateway.protocol.Requests.Initialize]].
    */
  case class InitializeParams(
    processId: Option[Int]         = None,
    clientInfo: Option[ClientInfo] = None,
    // Note [rootPath deprecated]
    rootPath: Option[String]                             = None,
    rootUri: Option[DocumentUri]                         = None,
    initializationOptions: Option[InitializationOptions] = None,
    capabilities: ClientCapabilities,
    trace: Option[Trace]                           = None,
    workspaceFolders: Option[Seq[WorkspaceFolder]] = None
  ) extends Params

  object InitializeParams {
    implicit val initializeParamsDecoder: Decoder[InitializeParams] =
      deriveDecoder
  }

  /* Note [rootPath deprecated]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~
   * `rootPath` is deprecated: use `rootUri`, LSP Spec.
   */

  /** Params of the notification
    * [[org.enso.gateway.protocol.Notifications.Initialized]].
    */
  case class InitializedParams() extends Params

  object InitializedParams {
    implicit val initializedParamsDecoder: Decoder[InitializedParams] =
      deriveDecoder
  }

  /** Array params. */
  case class Array(value: Seq[Option[Param]]) extends Params

  object Array {
    implicit val paramsArrayDecoder: Decoder[Array] =
      deriveUnwrappedDecoder
  }
}
