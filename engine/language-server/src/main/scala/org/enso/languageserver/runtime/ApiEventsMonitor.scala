package org.enso.languageserver.runtime

import org.enso.languageserver.monitoring.EventsMonitor
import org.enso.polyglot.runtime.Runtime.ApiEnvelope
import org.enso.polyglot.runtime.Runtime.Api

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Clock

/** Gather messages between the language server and the runtime and write them
  * to the provided file in CSV format.
  *
  * @param path the path where to write the events
  * @param clock the system clock
  */
final class ApiEventsMonitor(path: Path, clock: Clock) extends EventsMonitor {

  import ApiEventsMonitor.Direction

  /** @inheritdoc */
  override def registerEvent(event: Any): Unit =
    event match {
      case envelope: ApiEnvelope =>
        registerApiEvent(envelope)
      case RuntimeConnector.MessageFromRuntime(envelope) =>
        registerApiEvent(envelope)
      case _ =>
    }

  def registerApiEvent(event: ApiEnvelope): Unit =
    event match {
      case Api.Request(requestId, payload) =>
        Files.write(
          path,
          entry(Direction.Request, requestId, payload.getClass),
          StandardOpenOption.APPEND,
          StandardOpenOption.SYNC
        )

      case Api.Response(correlationId, payload) =>
        Files.write(
          path,
          entry(Direction.Response, correlationId, payload.getClass),
          StandardOpenOption.APPEND,
          StandardOpenOption.SYNC
        )
    }

  private def entry(
    direction: Direction,
    requestId: Option[Api.RequestId],
    payload: Class[_]
  ): Array[Byte] = {
    val requestIdEntry = requestId.fold("")(_.toString)
    val payloadEntry   = payload.getSimpleName
    val timeEntry      = clock.instant()
    s"$timeEntry,$direction,$requestIdEntry,$payloadEntry${System.lineSeparator()}"
      .getBytes(StandardCharsets.UTF_8)
  }
}
object ApiEventsMonitor {

  /** Create default instance of [[ApiEventsMonitor]]. */
  def apply(): ApiEventsMonitor =
    new ApiEventsMonitor(
      Files.createTempFile("enso-api-events-", ".csv"),
      Clock.systemUTC()
    )

  /** Direction of the message. */
  sealed trait Direction
  object Direction {
    case object Request  extends Direction
    case object Response extends Direction
  }
}
