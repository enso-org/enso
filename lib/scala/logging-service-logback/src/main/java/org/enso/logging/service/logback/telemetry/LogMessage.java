package org.enso.logging.service.logback.telemetry;

public record LogMessage(String loggerName, String message, Object[] arguments) {}
