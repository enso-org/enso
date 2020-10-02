package org.enso.loggingservice.internal.protocol

import io.circe.syntax._
import io.circe._

/**
  * Represents a language-agnostic exception that can be sent over the WebSocket
  * connection.
  *
  * @param name name of the exception, corresponds to the qualified class name
  *             of the exception
  * @param message message as returned by the exception's getMessage method
  * @param stackTrace serialized stack trace
  * @param cause optional serialized exception that caused this one; it must not
  *              point to itself, as this structure cannot be cyclic so that it
  *              can be serialized into JSON
  */
case class SerializedException(
  name: String,
  message: String,
  stackTrace: Seq[SerializedException.TraceElement],
  cause: Option[SerializedException]
)

object SerializedException {

  /**
    * Creates a [[SerializedException]] with a cause.
    */
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

  /**
    * Creates a [[SerializedException]] without a cause.
    */
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

  /**
    * Creates a [[SerializedException]] from a [[Throwable]].
    */
  def apply(throwable: Throwable): SerializedException =
    fromException(throwable)

  /**
    * Encodes a JVM [[Throwable]] as [[SerializedException]].
    */
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

  /**
    * Regular expression used to parse the stack trace elements format used by
    * [[StackTraceElement#toString]].
    *
    * It assumes that the stack trace element has form `element(location)`, for
    * example `foo.bar(Foo.java:123)` or `win32.foo(Native Method)`.
    *
    * This is the most robust way to get the location from the
    * [[StackTraceElement]] without duplicating the standard library code. In
    * case that the result of [[StackTraceElement#toString]] does not match the
    * regex, an approximation based on its getters is used.
    */
  private val stackTraceRegex = "(.*)\\((.*)\\)".r

  /**
    * Encodes a [[StackTraceElement]] as [[TraceElement]].
    */
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

  /**
    * Represents an element of a stack trace attached to the
    * [[SerializedException]].
    *
    * @param element name of the stack location; for example, in Java this is
    *                the qualified method name
    * @param location code location of the element
    */
  case class TraceElement(element: String, location: String)

  private object JsonFields {
    val Name       = "name"
    val Message    = "message"
    val StackTrace = "trace"
    val Cause      = "cause"

    object TraceElement {
      val Element  = "element"
      val Location = "location"
    }
  }

  /**
    * [[Encoder]] instance for [[SerializedException]].
    */
  implicit val encoder: Encoder[SerializedException] = encodeException

  /**
    * Encodes a [[SerializedException]] as its JSON representation.
    */
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

  /**
    * [[Decoder]] instance for [[SerializedException]].
    */
  implicit def decoder: Decoder[SerializedException] = decodeException

  /**
    * Tries to decode a [[SerializedException]] from its JSON representation.
    */
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

  /**
    * [[Encoder]] instance for [[TraceElement]].
    */
  implicit val traceEncoder: Encoder[TraceElement] = { traceElement =>
    Json.obj(
      JsonFields.TraceElement.Element  -> traceElement.element.asJson,
      JsonFields.TraceElement.Location -> traceElement.location.asJson
    )
  }

  /**
    * [[Decoder]] instance for [[TraceElement]].
    */
  implicit val traceDecoder: Decoder[TraceElement] = { json =>
    for {
      element  <- json.get[String](JsonFields.TraceElement.Element)
      location <- json.get[String](JsonFields.TraceElement.Location)
    } yield TraceElement(element = element, location = location)
  }
}
