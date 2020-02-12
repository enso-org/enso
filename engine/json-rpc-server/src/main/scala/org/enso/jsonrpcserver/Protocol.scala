package org.enso.jsonrpcserver
import io.circe.{Decoder, Encoder, Json}

/**
  * Represents valid JSON RPC request ids.
  */
sealed trait Id

object Id {
  import io.circe.syntax._
  import cats.syntax.functor._

  case class String(id: scala.Predef.String) extends Id
  case class Number(id: Int)                 extends Id

  implicit val encoder: Encoder[Id] = {
    case String(id) => id.asJson
    case Number(id) => id.asJson
  }
  implicit val decoder: Decoder[Id] = Decoder[scala.Predef.String]
    .map(String)
    .widen[Id]
    .or(Decoder[Int].map(Number).widen[Id])
}

/**
  * A superclass for supported API methods.
  * @param name the method name.
  */
abstract class Method(val name: String)

/**
  * A superclass for any custom type that can be carried either by a request
  * or a corresponding response for a given method.
  * @tparam M the method using these payloads.
  */
trait PayloadOf[+M]

/**
  * A superclass for parameters of a given method.
  * @tparam M the method using these params.
  */
trait ParamsOf[+M] extends PayloadOf[M]

/**
  * A superclass for results of a given method.
  * @tparam M the method using these response results.
  */
trait ResultOf[+M] extends PayloadOf[M]

/**
  * The basic JSON RPC request type.
  */
case class Request[+M <: Method](
  method: M,
  id: Id,
  params: ParamsOf[M]
)

/**
  * The basic JSON RPC notification type.
  */
case class Notification[+M <: Method](method: M, params: ParamsOf[M])

/**
  * The basic JSON RPC successful response type.
  */
case class ResponseResult[+M <: Method](
  id: Option[Id],
  data: ResultOf[M]
)

/**
  * The basic JSON RPC error response type.
  */
case class ResponseError(id: Option[Id], error: Error)

/**
  * An unknown result. Used when the upstream client sends an unexpected
  * response format.
  * @param result the bare json carried by the response.
  */
case class UnknownResult(result: Json) extends ResultOf[Method]

/**
  * A basic error type for responses.
  * @param code the error code.
  * @param message the error message.
  */
abstract class Error(val code: Int, val message: String)

/**
  * Builtin error types, defined by JSON RPC.
  */
object Errors {
  case object ParseError     extends Error(-32700, "Parse error")
  case object InvalidRequest extends Error(-32600, "Invalid Request")
  case object MethodNotFound extends Error(-32601, "Method not found")
  case object InvalidParams  extends Error(-32602, "Invalid params")
  case class UnknownError(override val code: Int, override val message: String)
      extends Error(code, message)
}

/**
  * A description containing all the supported methods and ways of serializing
  * their params and results.
  *
  * @param methods all the supported methods.
  * @param paramsDecoders decoders for all supported methods' params.
  * @param resultDecoders decoders for all supported request methods' results.
  * @param customErrors custom datatypes used for error codes.
  * @param payloadsEncoder an encoder for any payload (i.e. params or results)
  *                        used within this protocol.
  */
case class Protocol(
  methods: Set[Method],
  paramsDecoders: Map[Method, Decoder[ParamsOf[Method]]],
  resultDecoders: Map[Method, Decoder[ResultOf[Method]]],
  customErrors: Map[Int, Error],
  payloadsEncoder: Encoder[PayloadOf[Method]]
) {
  private val builtinErrors: Map[Int, Error] = List(
    Errors.ParseError,
    Errors.InvalidRequest,
    Errors.MethodNotFound,
    Errors.InvalidParams
  ).map(err => err.code -> err).toMap

  private val methodsMap: Map[String, Method] =
    methods.map(tag => (tag.name, tag)).toMap

  /**
    * Resolves a method by name.
    *
    * @param name the method name.
    * @return an object representing the method, if exists.
    */
  def resolveMethod(name: String): Option[Method] = methodsMap.get(name)

  /**
    * Looks up a params decoder for a given method.
    *
    * @param method the method to lookup decoder for.
    * @return the params decoder, if found.
    */
  def getParamsDecoder(
    method: Method
  ): Option[Decoder[ParamsOf[Method]]] =
    paramsDecoders.get(method)

  /**
    * Looks up a result decoder for a given method.
    *
    * @param method the method to lookup decoder for.
    * @return the result decoder, if found.
    */
  def getResultDecoder(method: Method): Option[Decoder[ResultOf[Method]]] =
    resultDecoders.get(method)

  /**
    * Looks up a proper error object by error code.
    *
    * @param code the error code to look up.
    * @return the corresponding custom error object, if exists.
    */
  def resolveError(code: Int): Option[Error] =
    builtinErrors.get(code).orElse(customErrors.get(code))
}
