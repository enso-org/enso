package org.enso.loggingservice.internal

import io.circe.{Decoder, Encoder, HCursor, Json, JsonObject}
import io.circe.syntax._

case class SerializedException(
  name: String,
  message: String,
  stackTrace: Seq[SerializedException.TraceElement],
  cause: Option[SerializedException]
)
object SerializedException {
  def apply(
    name: String,
    message: String,
    stackTrace: Seq[SerializedException.TraceElement],
    cause: SerializedException
  ): SerializedException =
    SerializedException(
      name       = name,
      message    = message,
      stackTrace = stackTrace,
      cause      = Some(cause)
    )

  def apply(
    name: String,
    message: String,
    stackTrace: Seq[SerializedException.TraceElement]
  ): SerializedException =
    SerializedException(
      name       = name,
      message    = message,
      stackTrace = stackTrace,
      cause      = None
    )

  def apply(throwable: Throwable): SerializedException =
    fromException(throwable)

  def fromException(throwable: Throwable): SerializedException = {
    val clazz = throwable.getClass
    val cause =
      if (throwable.getCause == throwable) None
      else Option(throwable.getCause).map(fromException)
    SerializedException(
      name       = Option(clazz.getCanonicalName).getOrElse(clazz.getName),
      message    = throwable.getMessage,
      stackTrace = throwable.getStackTrace.toSeq.map(encodeStackTraceElement),
      cause      = cause
    )
  }

  private val stackTraceRegex = "(.*)\\((.*)\\)".r
  private def encodeStackTraceElement(
    stackTraceElement: StackTraceElement
  ): TraceElement = {
    stackTraceElement.toString match {
      case stackTraceRegex(element, location) =>
        TraceElement(element = element, location = location)
      case _ =>
        val location = for {
          filename <- Option(stackTraceElement.getFileName)
          line <-
            if (stackTraceElement.getLineNumber < 0) None
            else Some(stackTraceElement.getLineNumber)
        } yield s"$filename:$line"
        val className  = stackTraceElement.getClassName
        val methodName = stackTraceElement.getMethodName
        TraceElement(
          s"$className.$methodName",
          location.getOrElse("Unknown Source")
        )
    }
  }

  case class TraceElement(element: String, location: String)

  object JsonFields {
    val Name       = "name"
    val Message    = "message"
    val StackTrace = "trace"
    val Cause      = "cause"

    object TraceElement {
      val Element  = "element"
      val Location = "location"
    }
  }

  implicit val encoder: Encoder[SerializedException] = encodeException

  private def encodeException(exception: SerializedException): Json = {
    val base = JsonObject(
      JsonFields.Name       -> exception.name.asJson,
      JsonFields.Message    -> exception.message.asJson,
      JsonFields.StackTrace -> exception.stackTrace.asJson
    )

    val result = exception.cause match {
      case Some(cause) =>
        base.+:((JsonFields.Cause, encodeException(cause)))
      case None =>
        base
    }

    result.asJson
  }

  implicit def decoder: Decoder[SerializedException] = decodeException

  private def decodeException(
    json: HCursor
  ): Decoder.Result[SerializedException] = {
    for {
      name       <- json.get[String](JsonFields.Name)
      message    <- json.get[String](JsonFields.Message)
      stackTrace <- json.get[Seq[TraceElement]](JsonFields.StackTrace)
      cause <-
        json.getOrElse[Option[SerializedException]](JsonFields.Cause)(None)
    } yield SerializedException(
      name       = name,
      message    = message,
      stackTrace = stackTrace,
      cause      = cause
    )
  }

  implicit val traceEncoder: Encoder[TraceElement] = { traceElement =>
    Json.obj(
      JsonFields.TraceElement.Element  -> traceElement.element.asJson,
      JsonFields.TraceElement.Location -> traceElement.location.asJson
    )
  }

  implicit val traceDecoder: Decoder[TraceElement] = { json =>
    for {
      element  <- json.get[String](JsonFields.TraceElement.Element)
      location <- json.get[String](JsonFields.TraceElement.Location)
    } yield TraceElement(element = element, location = location)
  }
}
