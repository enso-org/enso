package org.enso.jsonrpc

import io.circe.Decoder.Result
import io.circe._

/** An intermediate representation of JSON RPC messages, used for
  * (de)serialization.
  */
object JsonProtocol {
  import io.circe.generic.auto._
  import io.circe.syntax._

  /** Supertype of all conceivable messages received from the web.
    */
  sealed trait JsonMessage

  /** A bare notification object.
    */
  case class Notification(method: String, params: Json) extends JsonMessage

  /** A bare request object.
    */
  case class Request(method: String, id: Id, params: Json) extends JsonMessage

  /** A successful response object.
    */
  case class ResponseResult(id: Id, result: Json) extends JsonMessage

  /** An error response object.
    */
  case class ResponseError(id: Option[Id], error: ErrorData) extends JsonMessage

  /** The error response details. */
  case class ErrorData(code: Int, message: String, payload: Option[Json])

  object Constants {
    val jsonrpc: String              = "jsonrpc"
    val jsonrpcVersion: String       = "2.0"
    val jsonrpcField: (String, Json) = jsonrpc -> jsonrpcVersion.asJson
    val id: String                   = "id"
    val result: String               = "result"
    val method: String               = "method"
    val params: String               = "params"
    val error: String                = "error"
    val code: String                 = "code"
    val message: String              = "message"
    val payload: String              = "payload"
  }

  implicit val notificationEncoder: Encoder[Notification] =
    (notification: Notification) =>
      Json.obj(
        Constants.jsonrpcField,
        Constants.method -> notification.method.asJson,
        Constants.params -> notification.params
      )

  implicit val notificationDecoder: Decoder[Notification] = { c =>
    for {
      method <- c.downField(Constants.method).as[String]
      params = c.downField(Constants.params).focus.getOrElse(Json.Null)
    } yield Notification(method, params)
  }

  implicit val responseEncoder: Encoder[ResponseResult] =
    (response: ResponseResult) =>
      Json.obj(
        Constants.jsonrpcField,
        Constants.id     -> response.id.asJson,
        Constants.result -> response.result
      )

  implicit val errorEncoder: Encoder[ResponseError] =
    (response: ResponseError) =>
      Json.obj(
        Constants.jsonrpcField,
        Constants.id    -> response.id.asJson,
        Constants.error -> response.error.asJson
      )

  implicit val errorDataEncoder: Encoder[ErrorData] =
    (errorData: ErrorData) => {
      val base = JsonObject(
        Constants.code    -> errorData.code.asJson,
        Constants.message -> errorData.message.asJson
      )
      val result = errorData.payload match {
        case Some(additionalPayload) =>
          base.+:(Constants.payload -> additionalPayload)
        case None =>
          base
      }
      result.asJson
    }

  implicit val requestEncoder: Encoder[Request] = (request: Request) =>
    Json.obj(
      Constants.jsonrpcField,
      Constants.id     -> request.id.asJson,
      Constants.method -> request.method.asJson,
      Constants.params -> request.params
    )

  implicit val requestDecoder: Decoder[Request] = { c =>
    for {
      id     <- c.downField(Constants.id).as[Id]
      method <- c.downField(Constants.method).as[String]
      params = c.downField(Constants.params).focus.getOrElse(Json.Null)
    } yield Request(method, id, params)
  }

  implicit val bareMessageEncoder: Encoder[JsonMessage] = {
    case request: Request               => request.asJson
    case responseError: ResponseError   => responseError.asJson
    case responseResult: ResponseResult => responseResult.asJson
    case notification: Notification     => notification.asJson
  }

  implicit val decoder: Decoder[JsonMessage] = new Decoder[JsonMessage] {
    val expectedNotificationKeys: Set[String] =
      Set(Constants.jsonrpc, Constants.method)
    val expectedRequestKeys: Set[String] =
      Set(Constants.jsonrpc, Constants.method, Constants.id)
    val expectedResponseResultKeys: Set[String] =
      Set(Constants.jsonrpc, Constants.id, Constants.result)
    val expectedResponseErrorKeys: Set[String] =
      Set(Constants.jsonrpc, Constants.id, Constants.error)

    override def apply(c: HCursor): Result[JsonMessage] = {
      val jsonRpcValid = c
        .downField(Constants.jsonrpc)
        .as[String] == Right(Constants.jsonrpcVersion)
      if (!jsonRpcValid) {
        return Left(
          DecodingFailure("Invalid JSON RPC version manifest.", List())
        )
      }
      val fields = c.keys.getOrElse(List()).toSet
      if (expectedRequestKeys.subsetOf(fields)) {
        c.as[Request]
      } else if (expectedNotificationKeys.subsetOf(fields)) {
        c.as[Notification]
      } else if (fields == expectedResponseResultKeys) {
        c.as[ResponseResult]
      } else if (fields == expectedResponseErrorKeys) {
        c.as[ResponseError]
      } else {
        Left(DecodingFailure("Malformed JSON RPC message.", List()))
      }
    }
  }

  /** Parses a string into a valid JSON RPC message.
    * @param str the string to parse.
    * @return the data type corresponding to the message, if parsed
    *         successfully.
    */
  def parse(str: String): Option[JsonMessage] = {
    io.circe.parser.parse(str).toOption.flatMap(_.as[JsonMessage].toOption)
  }

  /** Encodes a message into a proper JSON RPC string.
    * @param msg the message to encode.
    * @return a string representing a valid JSON RPC package.
    */
  def encode(msg: JsonMessage): String = msg.asJson.noSpaces

}
