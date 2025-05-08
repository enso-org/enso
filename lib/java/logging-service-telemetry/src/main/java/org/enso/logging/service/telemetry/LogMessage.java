package org.enso.logging.service.telemetry;

import java.util.Arrays;

/** Created from {@link ch.qos.logback.classic.spi.ILoggingEvent}. */
public record LogMessage(String loggerName, String message, Object[] arguments, String logLevel) {

  @Override
  public String toString() {
    var args = arguments() == null ? "[]" : Arrays.asList(arguments());
    return "LogMessage[loggerName="
        + loggerName()
        + ", message="
        + message()
        + ", arguments="
        + args
        + "]";
  }
}
