package org.enso.languageserver.runtime.events;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.time.Clock;
import java.time.Instant;
import java.util.Base64;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.XMLFormatter;
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

  private final PrintStream out;
  private final Clock clock;

  private static final XMLFormatter EVENT_FORMAT = new XMLFormatter();
  private static final String XML_TAG = "<?xml version='1.0'?>";
  private static final String RECORDS_TAG_OPEN = "<records>";
  private static final String RECORDS_TAG_CLOSE = "</records>";
  private static final String MESSAGE_SEPARATOR = ",";
  private static final String MESSAGE_EMPTY_REQUEST_ID = "";
  private static final String HEARTBEAT_PATTERN = "\"method\": \"heartbeat/";

  /**
   * Create an instance of {@link RuntimeEventsMonitor}.
   *
   * @param out the output stream.
   * @param clock the system clock.
   */
  public RuntimeEventsMonitor(PrintStream out, Clock clock) {
    this.out = out;
    this.clock = clock;

    out.println(XML_TAG);
    out.println(RECORDS_TAG_OPEN);
  }

  /**
   * Create an instance of {@link RuntimeEventsMonitor}.
   *
   * @param out the output stream.
   */
  public RuntimeEventsMonitor(PrintStream out) {
    this(out, Clock.systemUTC());
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
    String entry = buildEntry(Direction.REQUEST, Option.empty(), message);
    out.print(entry);
  }

  @Override
  public void registerBinaryRpcMessage(ByteBuffer message) {
    byte[] bytes = new byte[message.remaining()];
    message.get(bytes);
    String payload = Base64.getEncoder().encodeToString(bytes);
    String entry = buildEntry(Direction.REQUEST, Option.empty(), payload);
    out.print(entry);
  }

  @Override
  public void close() throws IOException {
    out.println(RECORDS_TAG_CLOSE);
    out.close();
  }

  private void registerApiEnvelope(Runtime.ApiEnvelope event) {
    if (event instanceof Runtime$Api$Request request) {
      String payload = request.payload().getClass().getSimpleName();
      String entry = buildEntry(Direction.REQUEST, request.requestId(), payload);
      out.print(entry);
    } else if (event instanceof Runtime$Api$Response response) {
      String payload = response.payload().getClass().getSimpleName();
      String entry = buildEntry(Direction.RESPONSE, response.correlationId(), payload);
      out.print(entry);
    }
  }

  private String buildEntry(Direction direction, Option<UUID> requestId, String payload) {
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

    LogRecord record = new LogRecord(Level.INFO, message);
    record.setInstant(timeEntry);

    return EVENT_FORMAT.format(record);
  }
}
