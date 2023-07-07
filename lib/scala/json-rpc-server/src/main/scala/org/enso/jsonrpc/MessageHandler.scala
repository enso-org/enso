package org.enso.jsonrpc

import akka.actor.{Actor, ActorRef, Stash}
import io.circe.Json
import org.enso.jsonrpc.Errors.InvalidParams

/** An actor responsible for passing parsed massages between the web and
  * a controller actor.
  * @param protocol a factory for retrieving protocol object describing supported messages and their
  *                 serialization modes.
  * @param controller the controller actor, handling parsed messages.
  */
class MessageHandler(protocolFactory: ProtocolFactory, controller: ActorRef)
    extends Actor
    with Stash {

  private def getProtocol(): Protocol = protocolFactory.getProtocol()

  /** A pre-initialization behavior, awaiting a to-web connection end.
    * @return the actor behavior.
    */
  override def receive: Receive = {
    case MessageHandler.Connected(webConnection) =>
      unstashAll()
      context.become(established(webConnection, Map()))
    case _ => stash()
  }

  /** A fully established connection behavior.
    * @param webConnection the to-web connection end.
    * @param awaitingResponses a list of all requests sent to web, retained for
    *                          response deserialization.
    * @return the connected actor behavior.
    */
  def established(
    webConnection: ActorRef,
    awaitingResponses: Map[Id, Method]
  ): Receive = {
    case MessageHandler.WebMessage(msg) =>
      handleWebMessage(msg, webConnection, awaitingResponses)
    case MessageHandler.Disconnected =>
      controller ! MessageHandler.Disconnected
      context.stop(self)
    case request: Request[Method, Any] =>
      issueRequest(request, webConnection, awaitingResponses)
    case response: ResponseResult[Method, Any] =>
      issueResponseResult(response, webConnection)
    case response: ResponseError =>
      issueResponseError(response, webConnection)
    case notification: Notification[Method, Any] =>
      issueNotification(notification, webConnection)
  }

  private def issueResponseResult(
    response: ResponseResult[Method, Any],
    webConnection: ActorRef
  ): Unit = {
    val responseDataJson: Json = getProtocol().payloadsEncoder(response.data)
    val bareResp               = JsonProtocol.ResponseResult(response.id, responseDataJson)
    webConnection ! MessageHandler.WebMessage(JsonProtocol.encode(bareResp))
  }

  private def issueResponseError(
    response: ResponseError,
    webConnection: ActorRef
  ): Unit = {
    val bareError =
      JsonProtocol.ErrorData(
        response.error.code,
        response.error.message,
        response.error.payload
      )
    val bareResponse = JsonProtocol.ResponseError(response.id, bareError)
    webConnection ! MessageHandler.WebMessage(JsonProtocol.encode(bareResponse))
  }

  private def issueRequest(
    req: Request[Method, Any],
    webConnection: ActorRef,
    awaitingResponses: Map[Id, Method]
  ): Unit = {
    val paramsJson = getProtocol().payloadsEncoder(req.params)
    val bareReq    = JsonProtocol.Request(req.method.name, req.id, paramsJson)
    webConnection ! MessageHandler.WebMessage(JsonProtocol.encode(bareReq))
    context.become(
      established(webConnection, awaitingResponses + (req.id -> req.method))
    )
  }

  private def issueNotification(
    notification: Notification[Method, Any],
    webConnection: ActorRef
  ): Unit = {
    val paramsJson = getProtocol().payloadsEncoder(notification.params)
    val bareNotification =
      JsonProtocol.Notification(notification.method.name, paramsJson)
    webConnection ! MessageHandler.WebMessage(
      JsonProtocol.encode(bareNotification)
    )
  }

  private def handleWebMessage(
    msg: String,
    webConnection: ActorRef,
    awaitingResponses: Map[Id, Method]
  ): Unit = {
    val bareMsg = JsonProtocol.parse(msg)
    bareMsg match {
      case None =>
        webConnection ! MessageHandler.WebMessage(
          makeError(None, Errors.ParseError)
        )

      case Some(JsonProtocol.Request(methodName, id, params)) =>
        val decoder = resolveDecoder(methodName)
        val request =
          decoder.flatMap(_.buildRequest(id, params).toRight(InvalidParams))
        request match {
          case Left(error) =>
            webConnection ! MessageHandler.WebMessage(
              makeError(Some(id), error)
            )
          case Right(req) =>
            controller ! req
        }

      case Some(JsonProtocol.Notification(methodName, params)) =>
        val notification = resolveDecoder(methodName).flatMap(
          _.buildNotification(params).toRight(InvalidParams)
        )
        notification.foreach(controller ! _)

      case Some(JsonProtocol.ResponseResult(id, result)) =>
        val maybeDecoded: Option[Any] = for {
          method   <- awaitingResponses.get(id)
          decoder  <- getProtocol().getResultDecoder(method)
          response <- decoder.buildResponse(id, result)
        } yield response
        maybeDecoded.foreach(controller ! _)
        context.become(established(webConnection, awaitingResponses - id))

      case Some(JsonProtocol.ResponseError(mayId, bareError)) =>
        val error = getProtocol()
          .resolveError(bareError.code)
          .getOrElse(
            Errors
              .UnknownError(
                bareError.code,
                bareError.message,
                bareError.payload
              )
          )
        controller ! ResponseError(mayId, error)
        mayId.foreach(id =>
          context.become(established(webConnection, awaitingResponses - id))
        )

    }
  }

  private def makeError(id: Option[Id], error: Error): String = {
    val bareError =
      JsonProtocol.ErrorData(error.code, error.message, error.payload)
    val bareErrorResponse = JsonProtocol.ResponseError(id, bareError)
    JsonProtocol.encode(bareErrorResponse)
  }

  private def resolveDecoder(
    methodName: String
  ): Either[Error, ParamsDecoder[Method, Any]] = {
    for {
      method <- getProtocol()
        .resolveMethod(methodName)
        .toRight(protocolFactory.onMissingMethod())
      decoder <- getProtocol()
        .getParamsDecoder(method)
        .toRight(Errors.InvalidRequest)
    } yield decoder
  }
}

/** Control messages for the [[MessageHandler]] actor.
  */
object MessageHandler {

  /** A message exchanged on the Web side of the boundary.
    *
    * @param message the serialized json contents of the message.
    */
  case class WebMessage(message: String)

  /** A control message used for [[MessageHandler]] initializations
    * @param webConnection the actor representing the web.
    */
  case class Connected(webConnection: ActorRef)

  /** A control message usef to notify the controller about
    * the connection being closed.
    */
  case object Disconnected
}
