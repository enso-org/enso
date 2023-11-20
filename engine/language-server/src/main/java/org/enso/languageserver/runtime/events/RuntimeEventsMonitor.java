package org.enso.languageserver.runtime.events;

import java.io.IOException;
import java.io.PrintStream;
import java.time.Clock;
import java.time.Instant;
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

  /**
   * Direction of the message.
   */
  private enum Direction {
    REQUEST,
    RESPONSE
  }

  @Override
  public void registerEvent(Object event) {
    if (event instanceof Runtime.ApiEnvelope envelope) {
      registerApiEnvelope(envelope);
    } else if (event instanceof RuntimeConnector.MessageFromRuntime messageFromRuntime) {
      registerApiEnvelope(messageFromRuntime.message());
    }
  }

  @Override
  public void close() throws IOException {
    out.println(RECORDS_TAG_CLOSE);
    out.close();
  }

  private void registerApiEnvelope(Runtime.ApiEnvelope event) {
    if (event instanceof Runtime$Api$Request request) {
      String entry =
          buildEntry(Direction.REQUEST, request.requestId(), request.payload().getClass());
      out.print(entry);
    } else if (event instanceof Runtime$Api$Response response) {
      String entry =
          buildEntry(Direction.RESPONSE, response.correlationId(), response.payload().getClass());
      out.print(entry);
    }
  }

  private String buildEntry(Direction direction, Option<UUID> requestId, Class<?> payload) {
    String requestIdEntry = requestId.fold(() -> MESSAGE_EMPTY_REQUEST_ID, UUID::toString);
    String payloadEntry = payload.getSimpleName();
    Instant timeEntry = clock.instant();

    String message =
        new StringBuilder()
            .append(direction)
            .append(MESSAGE_SEPARATOR)
            .append(requestIdEntry)
            .append(MESSAGE_SEPARATOR)
            .append(payloadEntry)
            .toString();

    LogRecord record = new LogRecord(Level.INFO, message);
    record.setInstant(timeEntry);

    return EVENT_FORMAT.format(record);
  }
}
