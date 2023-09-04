package org.enso.logging;

public class LoggerInitializationFailed extends RuntimeException {
  public LoggerInitializationFailed() {
    super("Logger initialization failed");
  }
}
