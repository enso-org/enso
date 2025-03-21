package org.enso.logging.service.logback.telemetry;

/** Created from {@link ch.qos.logback.classic.spi.ILoggingEvent}. */
public record LogMessage(String loggerName, String message, Object[] arguments) {}
