package org.enso.languageserver.runtime.events;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.time.Clock;
import java.time.Instant;
import java.util.Base64;
import java.util.UUID;
import java.util.function.BiConsumer;
import org.enso.languageserver.runtime.RuntimeConnector;
import org.enso.polyglot.runtime.Runtime;
import org.enso.polyglot.runtime.Runtime$Api$Request;
import org.enso.polyglot.runtime.Runtime$Api$Response;
import org.enso.profiling.events.EventsMonitor;
import scala.Option;

/**
 * Gather messages between the language server and the runtime and write them to the provided file
 * in XML format.
 */
public final class RuntimeEventsMonitor implements EventsMonitor {

  private final BiConsumer<Instant, String> log;
  private final Clock clock;

  private static final String MESSAGE_SEPARATOR = ",";
  private static final String MESSAGE_EMPTY_REQUEST_ID = "";
  private static final String HEARTBEAT_PATTERN = "\"method\": \"heartbeat/";

  /**
   * Create an instance of {@link RuntimeEventsMonitor}.
   *
   * @param log the logger
   * @param clock the system clock.
   */
  public RuntimeEventsMonitor(BiConsumer<Instant, String> log, Clock clock) {
    this.log = log;
    this.clock = clock;
  }

  /**
   * Create an instance of {@link RuntimeEventsMonitor}.
   *
   * @param log the logger
   */
  public RuntimeEventsMonitor(BiConsumer<Instant, String> log) {
    this(log, Clock.systemUTC());
  }

  /** Direction of the message. */
  private enum Direction {
    REQUEST,
    RESPONSE
  }

  @Override
  public void registerRuntimeMessage(Object event) {
    if (event instanceof Runtime.ApiEnvelope envelope) {
      registerApiEnvelope(envelope);
    } else if (event instanceof RuntimeConnector.MessageFromRuntime messageFromRuntime) {
      registerApiEnvelope(messageFromRuntime.message());
    }
  }

  @Override
  public void registerTextRpcMessage(String message) {
    if (message.contains(HEARTBEAT_PATTERN)) return;
    logEntry(Direction.REQUEST, Option.empty(), message);
  }

  @Override
  public void registerBinaryRpcMessage(ByteBuffer message) {
    byte[] bytes = new byte[message.remaining()];
    message.get(bytes);
    String payload = Base64.getEncoder().encodeToString(bytes);
    logEntry(Direction.REQUEST, Option.empty(), payload);
  }

  @Override
  public void close() throws IOException {}

  private void registerApiEnvelope(Runtime.ApiEnvelope event) {
    if (event instanceof Runtime$Api$Request request) {
      String payload = request.payload().getClass().getSimpleName();
      logEntry(Direction.REQUEST, request.requestId(), payload);
    } else if (event instanceof Runtime$Api$Response response) {
      String payload = response.payload().getClass().getSimpleName();
      logEntry(Direction.RESPONSE, response.correlationId(), payload);
    }
  }

  private void logEntry(Direction direction, Option<UUID> requestId, String payload) {
    String requestIdEntry = requestId.fold(() -> MESSAGE_EMPTY_REQUEST_ID, UUID::toString);
    Instant timeEntry = clock.instant();

    String message =
        new StringBuilder()
            .append(direction)
            .append(MESSAGE_SEPARATOR)
            .append(requestIdEntry)
            .append(MESSAGE_SEPARATOR)
            .append(payload)
            .toString();

    log.accept(timeEntry, message);
  }
}
