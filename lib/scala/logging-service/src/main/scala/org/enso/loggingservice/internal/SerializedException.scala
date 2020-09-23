package org.enso.loggingservice.internal

case class SerializedException(
  name: String,
  message: String,
  stackTrace: Seq[SerializedException.TraceElement],
  cause: Option[SerializedException]
)
object SerializedException {
  case class TraceElement(stackLocation: String, fileLocation: String)
}
