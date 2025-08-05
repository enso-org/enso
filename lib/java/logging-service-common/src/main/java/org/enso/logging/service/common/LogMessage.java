package org.enso.logging.service.common;

import java.util.Arrays;
import java.util.Map;

/** Created from {@link ch.qos.logback.classic.spi.ILoggingEvent}. */
public record LogMessage(
    String loggerName,
    String message,
    Object[] arguments,
    String logLevel,
    Map<String, String> mdc) {

  public LogMessage(String loggerName, String message, Object[] arguments) {
    this(loggerName, message, arguments, "TRACE", null);
  }

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
