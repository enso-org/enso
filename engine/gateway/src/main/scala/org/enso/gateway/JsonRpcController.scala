package org.enso.gateway

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import io.circe
import io.circe.CursorOp.DownField
import io.circe.{DecodingFailure, ParsingFailure, Printer}
import io.circe.parser.decode
import io.circe.syntax._
import org.enso.gateway.protocol._
import org.enso.gateway.protocol.request.IdHolder
import org.enso.gateway.protocol.response.ResponseError.{
  InitializeError,
  MethodNotFoundError,
  ParseError,
  UnexpectedError
}
import org.enso.gateway.protocol.response.error.Data

import scala.concurrent.Future

object JsonRpcController {

  /** A string specifying the version of the JSON-RPC protocol.
    *
    * Must be exactly "2.0".
    *
    * @see LSP Spec:
    *      https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#contentPart
    * @see JSON-RPC Spec: https://www.jsonrpc.org/specification#request_object
    */
  val jsonRpcVersion = "2.0"
}

/** Helper for implementing protocol over text-based transport.
  *
  * Requests and responses are marshaled as text using JSON-RPC.
  * It handles and decodes all JSON-RPC messages and dispatch them to the
  * Gateway.
  *
  * @param gateway [[ActorRef]] of Gateway actor.
  */
class JsonRpcController(gateway: ActorRef)(implicit system: ActorSystem) {
  import system.dispatcher

  /** Generates text reply for given request text message, no reply for
    * notification.
    */
  def getTextOutput(
    input: String
  )(implicit timeout: Timeout): Future[Option[String]] = {
    val id = decode[IdHolder](input).map(_.id).toOption

    decode[RequestOrNotification](input) match {
      case Right(notification: Notification[_]) =>
        gateway ! notification
        Future.successful(None)

      case Right(request: Request[_]) => // Note [Response Id]
        val responseFuture =
          (gateway ? request).mapTo[Response]
        responseFuture.map(
          response => Some(encodeToJson(response))
        )

      case Left(err) =>
        Future.successful(
          Some(encodeToJson(mkErrorResponse(input, err).copy(id = id)))
        )
    }
  }

  /* Note [Response Id]
   * ~~~~~~~~~~~~~~~~~~
   * Gateway and language server are responsible for id of response being equal
   * to id of request.
   */

  private def mkErrorResponse(
    input: String,
    err: circe.Error
  ): Response =
    err match {
      case err: ParsingFailure =>
        mkParseErrorResponse(input, err)

      case DecodingFailure(_, List(DownField(Notification.jsonrpcField))) =>
        initializeErrorResponse

      case DecodingFailure(_, List(DownField(Notification.methodField))) =>
        methodNotFoundResponse

      case e =>
        mkUnexpectedErrorResponse(e)
    }

  private def mkUnexpectedErrorResponse(
    err: circe.Error
  ): Response = {
    Response.error(
      error = UnexpectedError(
        data = Some(
          Data.Text(err.toString)
        )
      )
    )
  }

  private val methodNotFoundResponse: Response = {
    Response.error(
      error = MethodNotFoundError()
    )
  }

  private val initializeErrorResponse: Response = {
    val defaultRetry = false
    Response.error(
      error = InitializeError(
        data = Some(
          Data
            .InitializeData(
              retry = defaultRetry
            )
        )
      )
    )
  }

  private def mkParseErrorResponse(
    input: String,
    err: ParsingFailure
  ): Response = {
    Response.error(
      error = ParseError(
        Some(
          Data
            .ParseData(
              input,
              err.toString
            )
        )
      )
    )
  }

  private def encodeToJson(
    response: Response
  ): String = {
    response.asJson.printWith(
      Printer.noSpaces.copy(dropNullValues = true)
    )
  }
}
