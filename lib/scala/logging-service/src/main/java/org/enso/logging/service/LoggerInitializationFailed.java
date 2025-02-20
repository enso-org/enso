package org.enso.logging.service;

public class LoggerInitializationFailed extends RuntimeException {
  public LoggerInitializationFailed() {
    super("Logger initialization failed");
  }
}
